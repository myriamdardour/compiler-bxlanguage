# --------------------------------------------------------------------
import contextlib as cl
import itertools as it

from typing import Optional as Opt

from .bxast   import *
from .bxscope import Scope
from .bxtac   import *

# ====================================================================
# Maximal munch

class MM:
    _counter = -1
    _decl_counter = 0

    PRINTS = {
        Type.INT  : 'print_int',
        Type.BOOL : 'print_bool',
    }

    def __init__(self):
        self._proc    = None
        self._tac     = []
        self._scope   = Scope()
        self._loops   = []
        self._proc_stack : list[ProcDecl] = [] 
        self._temp_map : dict[str, str] = {}  
        self._frame_maps : list[dict[str, object]] = []  
        self._temp_frames : dict[str, int] = {}

    tac = property(lambda self: self._tac)

    @staticmethod
    def mm(prgm: Program):
        mm = MM(); mm.for_program(prgm)
        return mm._tac

    @classmethod
    def fresh_temporary(cls):
        cls._counter += 1
        return f'%{cls._counter}'

    @classmethod
    def fresh_decl_id(cls):
        """Get a unique ID for function declarations."""
        cls._decl_counter += 1
        return cls._decl_counter
    
    @classmethod
    def mangle_name(cls, name: str, outer_name: str, depth: int, uid: int) -> str:
        """Mangle nested function name with outer procedure name, depth, and unique ID."""
        return f'{outer_name}__{name}__nest{depth}.{uid}'

    @property
    def _current_frame_index(self) -> int:
        """Return the index of the current stack frame (0 for main/top-level)."""
        return len(self._proc_stack)

    def _register_temp(self, temp: str):
        """Register a temp as belonging to the current frame."""
        self._temp_frames[temp] = self._current_frame_index
    
    def _temp_offset(self, t: str) -> int:
        """Compute frame offset for a temp string like '%12'."""
        if not t.startswith('%'):
            assert False, f"Expected temp string starting with %, got {t}"
        try:
            n = int(t[1:])
            return -8 * (n + 1)
        except ValueError:
            assert False, f"Failed to parse temp number from {t}"
    
    def _resolve_in_frames(self, name: str) -> tuple[int, str]:
        """Search frame maps from innermost to outermost.
        Returns (hops, temp) where hops=0 means current frame, hops=1 means parent, etc.
        Only handles entries whose value is a temp string (skips tuples)."""
        for i in range(len(self._frame_maps) - 1, -1, -1):
            frame_map = self._frame_maps[i]
            if name in frame_map:
                val = frame_map[name]
                if isinstance(val, str) and val.startswith('%'):
                    hops = len(self._frame_maps) - 1 - i
                    return (hops, val)
        assert False, f"Variable {name} not found in frame maps"
    
    def _follow_static_links(self, hops: int) -> str:
        """Follow static link chain for the given number of hops.
        Returns the temp that points to the target frame."""
        if hops == 0:
            assert False, "_follow_static_links called with hops=0"
        
        base = self.fresh_temporary()
        self.push('load_slink', result=base)
        
        for _ in range(1, hops):
            next_base = self.fresh_temporary()
            self.push('load_prev_slink', base, result=next_base)
            base = next_base
        
        return base
    
    def _get_variable_location(self, name: str) -> str:
        """
        Returns a temp that holds the value of the variable.
        Respects lexical scoping via self._scope and frame depth via self._temp_frames.
        """

        if name in self._scope:
            val = self._scope[name]
            if isinstance(val, str) and val.startswith('@'):
                return val
            

            target_temp = val
            

            if target_temp in self._temp_frames:
                owner_index = self._temp_frames[target_temp]
                current_index = self._current_frame_index
                hops = current_index - owner_index
                
                if hops == 0:
                    return target_temp
                else:

                    base_ptr = self._follow_static_links(hops)
                    offset = self._temp_offset(target_temp)
                    result_temp = self.fresh_temporary()
                    self.push('load_mem', base_ptr, offset, result=result_temp)
                    return result_temp
            
            return target_temp

        assert False, f"Variable {name} not found in scope"

    @classmethod
    def fresh_label(cls):
        cls._counter += 1
        return f'.L{cls._counter}'

    def push(
        self,
        opcode     : str,
        *arguments : str | int,
        result     : Opt[str] = None,
    ):
        self._proc.tac.append(TAC(opcode, list(arguments), result))

    def push_label(self, label: str):
        self._proc.tac.append(f'{label}:')

    @cl.contextmanager
    def in_loop(self, labels: tuple[str, str]):
        self._loops.append(labels)
        try:
            yield
        finally:
            self._loops.pop()

    def for_program(self, prgm: Program):

        for decl in prgm:
            match decl:
                case GlobVarDecl(name, init, type_):
                    assert(isinstance(init, IntExpression))
                    self._tac.append(TACVar(name.value, init.value))
                    self._scope.push(name.value, f'@{name.value}')
                case ProcDecl(name, _, _, _):

                    self._scope.push(name.value, ('proc', f'@{name.value}'))
        

        self._scope.push('print_int', ('proc', 'print_int'))
        self._scope.push('print_bool', ('proc', 'print_bool'))


        for decl in prgm:
            match decl:
                case ProcDecl(name, arguments, retty, body):
                    assert(self._proc is None)
                    self._proc_stack = []
                    self._temp_map = {}
                    self._frame_maps = []
                    self._temp_frames = {} 
                    
                    with self._scope.in_subscope():
                        frame_map = {}
                        self._frame_maps.append(frame_map)
                        
                        tac_args = []
                        for vnames, vtype_ in arguments:
                            for vname in vnames:
                               
                                if isinstance(vtype_, FunType):
                                    temp = self.fresh_temporary()
                                    self._register_temp(temp)
                                    tac_args.append(temp)
                                    self._scope.push(vname.value, temp)
                                else:
                                    temp = self.fresh_temporary()
                                    self._register_temp(temp)
                                    tac_args.append(temp)
                                    self._scope.push(vname.value, temp)
                                    self._temp_map[vname.value] = temp
                                    frame_map[vname.value] = temp

                        self._proc = TACProc(
                            name      = name.value,
                            arguments = tac_args,
                        )

                        self.for_statement(body)

                        if name.value == 'main':
                            self.for_statement(ReturnStatement(IntExpression(0)));

                        self._tac.append(self._proc)
                        self._proc = None
                        self._proc_stack = []
                        self._temp_map = {}
                        self._frame_maps.pop()

    def for_block(self, block: Block):
        with self._scope.in_subscope():
            for stmt in block:
                self.for_statement(stmt)

    def for_statement(self, stmt: Statement):
        match stmt:
            case VarDeclStatement(name, init):
                temp = self.fresh_temporary()
                self._register_temp(temp)
                self._scope.push(name.value, temp)
                self._temp_map[name.value] = temp
                if self._frame_maps:
                    self._frame_maps[-1][name.value] = temp
                init_temp = self.for_expression(init)
                self.push('copy', init_temp, result = temp)

            case AssignStatement(lhs, rhs):
                rhs_temp = self.for_expression(rhs)

                if lhs.value in self._scope:
                    lhs_val = self._scope[lhs.value]
                    
                    if isinstance(lhs_val, str) and lhs_val.startswith('@'):
                        self.push('copy', rhs_temp, result=lhs_val)
                    else:
                        target_temp = lhs_val
                        if target_temp in self._temp_frames:
                            owner_index = self._temp_frames[target_temp]
                            hops = self._current_frame_index - owner_index
                            
                            if hops == 0:
                                self.push('copy', rhs_temp, result=target_temp)
                            else:
                                base = self._follow_static_links(hops)
                                offset = self._temp_offset(target_temp)
                                self.push('store_mem', rhs_temp, base, offset)
                        else:
                             self.push('copy', rhs_temp, result=target_temp)

            case ExprStatement(expr):
                self.for_expression(expr)

            case IfStatement(condition, then, else_):
                tlabel = self.fresh_label()
                flabel = self.fresh_label()
                olabel = self.fresh_label()

                self.for_bexpression(condition, tlabel, flabel)
                self.push_label(tlabel)
                self.for_statement(then)
                self.push('jmp', olabel)
                self.push_label(flabel)
                if else_ is not None:
                    self.for_statement(else_)
                self.push_label(olabel)

            case WhileStatement(condition, body):
                clabel = self.fresh_label()
                blabel = self.fresh_label()
                olabel = self.fresh_label()

                with self.in_loop((clabel, olabel)):
                    self.push_label(clabel)
                    self.for_bexpression(condition, blabel, olabel)
                    self.push_label(blabel)
                    self.for_statement(body)
                    self.push('jmp', clabel)
                    self.push_label(olabel)

            case ContinueStatement():
                self.push('jmp', self._loops[-1][0])

            case BreakStatement():
                self.push('jmp', self._loops[-1][1])

            case BlockStatement(body):
                self.for_block(body)

            case ProcDeclStatement(decl):
                depth = len(self._proc_stack) + 1
                outer_name = self._proc.name if self._proc else 'main'
                
                uid = self.fresh_decl_id()
                mangled_name = self.mangle_name(decl.name.value, outer_name, depth, uid)
                
                saved_proc = self._proc
                
                self._scope.open()
                nested_frame_map = {}
                self._frame_maps.append(nested_frame_map)
                
                nested_tac_args = []
                self._proc_stack.append(decl)
                
                for vnames, vtype_ in decl.arguments:
                    for vname in vnames:
                        temp = self.fresh_temporary()
                        self._register_temp(temp)
                        nested_tac_args.append(temp)
                        self._scope.push(vname.value, temp)
                        nested_frame_map[vname.value] = temp
                
                nested_proc = TACProc(name=mangled_name, arguments=nested_tac_args)
                self._proc = nested_proc
                
                self.for_statement(decl.body)
                
                self._tac.append(nested_proc)
                
                self._proc = saved_proc
                self._proc_stack.pop()
                self._frame_maps.pop()
                self._scope.close()


                fat_ptr_addr = self.fresh_temporary()
                self._register_temp(fat_ptr_addr)
                self.push('alloc_closure', result=fat_ptr_addr)
                
                code_label = f'@{mangled_name}'
                self.push('store_code_ptr', code_label, fat_ptr_addr)
                
                fp_temp = self.fresh_temporary()
                self.push('frameptr', result=fp_temp)
                self.push('store_mem', fp_temp, fat_ptr_addr, 8)
                
                self._scope.push(decl.name.value, fat_ptr_addr)

            case ReturnStatement(expr):
                if expr is None:
                    self.push('ret')
                else:
                    temp = self.for_expression(expr)
                    self.push('ret', temp)

            case _:
                assert(False)

    def for_expression(self, expr: Expression, force = False):
        target = None

        if not force and expr.type_ == Type.BOOL:
            target = self.fresh_temporary()
            tlabel = self.fresh_label()
            flabel = self.fresh_label()

            self.push('const', 0, result = target)
            self.for_bexpression(expr, tlabel, flabel)
            self.push_label(tlabel)
            self.push('const', 1, result = target)
            self.push_label(flabel)

        else:
            match expr:
                case VarExpression(name):

                    if name.value in self._scope:
                        val = self._scope[name.value]
                        if isinstance(val, tuple) and val[0] == 'proc':

                            func_label = val[1]
                            target = self.fresh_temporary()
                            
                            self.push('alloc_closure', result=target)
                            self.push('store_code_ptr', func_label, target)
                            

                            zero_temp = self.fresh_temporary()
                            self.push('const', 0, result=zero_temp)
                            self.push('store_mem', zero_temp, target, 8)
                            
                            return target
                            
                    target = self._get_variable_location(name.value)

                case IntExpression(value):
                    target = self.fresh_temporary()
                    self.push('const', value, result = target)

                case OpAppExpression(operator, arguments):
                    target    = self.fresh_temporary()
                    arguments = [self.for_expression(e) for e in arguments]
                    self.push(OPCODES[operator], *arguments, result = target)

                case CallExpression(proc, arguments):
                    arg_temps = [self.for_expression(arg) for arg in arguments]
                    
                    if expr.type_ != Type.VOID:
                        target = self.fresh_temporary()
                    
                    is_closure = False
                    closure_temp = None
                    
                    if proc.value in self._scope:
                        val = self._scope[proc.value]
                        if isinstance(val, tuple) and val[0] == 'proc':
                            is_closure = False
                        elif isinstance(val, str) and val.startswith('@'):
                             pass 
                        else:
                             closure_temp = self._get_variable_location(proc.value)
                             is_closure = True

                    if is_closure:
                        slink_temp = self.fresh_temporary()
                        self.push('load_mem', closure_temp, 8, result=slink_temp)
                        
                        code_temp = self.fresh_temporary()
                        self.push('load_mem', closure_temp, 0, result=code_temp)
                        
                        self.push('param_slink', slink_temp)
                        for arg in arg_temps:
                            self.push('param', arg)
                        self.push('icall', code_temp, len(arg_temps), result=target)
                        
                    else:
                        self.push('param_slink', 0)
                        for arg in arg_temps:
                            self.push('param', arg)
                        self.push('call', proc.value, len(arg_temps), result=target)

                case PrintExpression(argument):
                    temp = self.for_expression(argument)
                    self.push('param_slink', 0)
                    self.push('param', temp)
                    proc = self.PRINTS[argument.type_]
                    self.push('call', proc, 1)

                case _:
                    assert(False)

        return target

    CMP_JMP = {
        'cmp-equal'                 : 'jz',
        'cmp-not-equal'             : 'jnz',
        'cmp-lower-than'            : 'jgt',
        'cmp-lower-or-equal-than'   : 'jge',
        'cmp-greater-than'          : 'jlt',
        'cmp-greater-or-equal-than' : 'jle',
    }

    def for_bexpression(self, expr: Expression, tlabel: str, flabel: str):
        assert(expr.type_ == Type.BOOL)

        match expr:
            case VarExpression(name):
                temp = self.for_expression(expr, force=True)
                self.push('jz', temp, flabel)
                self.push('jmp', tlabel)

            case BoolExpression(True):
                self.push('jmp', tlabel)

            case BoolExpression(False):
                self.push('jmp', flabel)

            case OpAppExpression(
                    'cmp-equal'                |
                    'cmp-not-equal'            |
                    'cmp-lower-than'           |
                    'cmp-lower-or-equal-than'  |
                    'cmp-greater-than'         |
                    'cmp-greater-or-equal-than',
                    [e1, e2]):

                t1 = self.for_expression(e1)
                t2 = self.for_expression(e2)
                t  = self.fresh_temporary()
                self.push(OPCODES['subtraction'], t2, t1, result = t)

                self.push(self.CMP_JMP[expr.operator], t, tlabel)
                self.push('jmp', flabel)

            case OpAppExpression('boolean-and', [e1, e2]):
                olabel = self.fresh_label()
                self.for_bexpression(e1, olabel, flabel)
                self.push_label(olabel)
                self.for_bexpression(e2, tlabel, flabel)

            case OpAppExpression('boolean-or', [e1, e2]):
                olabel = self.fresh_label()
                self.for_bexpression(e1, tlabel, olabel)
                self.push_label(olabel)
                self.for_bexpression(e2, tlabel, flabel)

            case OpAppExpression('boolean-not', [e]):
                self.for_bexpression(e, flabel, tlabel)

            case CallExpression(_):
                temp = self.for_expression(expr, force = True)
                self.push('jz', temp, flabel)
                self.push('jmp', tlabel)

            case _:
                assert(False)