# --------------------------------------------------------------------
import abc

from .bxtac import *

# --------------------------------------------------------------------
class AsmGen(abc.ABC):
    BACKENDS   = {}
    NAME       = None
    SYSTEM     = None
    MACHINE    = None

    def __init__(self):
        self._tparams = dict()
        self._temps   = dict()
        self._asm     = []
        self._pending_slink = 0

    def _temp(self, temp):
        if temp is None:
            return None
        if isinstance(temp, str) and temp.startswith('@'):
            prelude, temp = self._format_temp(temp[1:])
        elif temp in self._tparams:
            prelude, temp = [], self._format_param(self._tparams[temp])
        else:
            if isinstance(temp, str) and temp.startswith('%') and len(temp) > 1:
                try:
                    index = int(temp[1:])
                    self._temps[temp] = index
                    prelude, temp = self._format_temp(index)
                except ValueError:
                    index = self._temps.setdefault(temp, len(self._temps))
                    prelude, temp = self._format_temp(index)
            else:
                index = self._temps.setdefault(temp, len(self._temps))
                prelude, temp = self._format_temp(index)
        for i in prelude:
            self._emit(*i)
        return temp

    @abc.abstractmethod
    def _format_temp(self, index):
        pass

    @abc.abstractmethod
    def _format_param(self, index):
        pass

    def __call__(self, instr: TAC | str):
        if isinstance(instr, str):
            self._asm.append(instr)
            return

        opcode = instr.opcode
        args   = instr.arguments[:]

        if instr.result is not None:
            args.append(instr.result)

        if hasattr(self, f'_emit_{opcode}'):
            getattr(self, f'_emit_{opcode}')(*args)
        else:
            print(f"Warning: Unknown opcode {opcode}")

    def _get_asm(self, opcode, *args):
        if not args:
            return f'\t{opcode}'
        return f'\t{opcode}\t{", ".join(args)}'

    def _get_label(self, lbl):
        return f'{lbl}:'

    def _emit(self, opcode, *args):
        self._asm.append(self._get_asm(opcode, *args))

    def _emit_label(self, lbl):
        self._asm.append(self._get_label(lbl))

    @classmethod
    def get_backend(cls, name):
        return cls.BACKENDS[name]

    @classmethod
    def select_backend(cls, system: str, machine: str):
        for backend in cls.BACKENDS.values():
            if system == backend.SYSTEM and machine == backend.MACHINE:
                return backend
        return None

    @classmethod    
    def register(cls, backend):
        cls.BACKENDS[backend.NAME] = backend

    @classmethod
    def lower(cls, tacs: list[TACProc | TACVar]) -> str:
        aout = [cls.lower1(tac) for tac in tacs]
        aout = [x for tac in aout for x in tac]
        return "\n".join(aout) + "\n"

# ====================================================================
# x64 Linux Backend
# ====================================================================
class AsmGen_x64_Linux(AsmGen):
    NAME    = 'x64-linux'
    SYSTEM  = 'Linux'
    MACHINE = 'x86_64'
    PARAMS  = ['%rdi', '%rsi', '%rdx', '%rcx', '%r8', '%r9']

    def __init__(self):
        super().__init__()
        self._params = []
        self._endlbl = None

    def _format_temp(self, index):
        if isinstance(index, str):
            return [], f'{index}(%rip)'
        return [], f'-{8*(index+1)}(%rbp)'

    def _format_param(self, index):
        return f'{8*(index + 4)}(%rbp)'

    def _emit_const(self, ctt, dst): self._emit('movq', f'${ctt}', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_copy(self, src, dst): self._emit('movq', self._temp(src), '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_add(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('addq', self._temp(op2), '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_sub(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('subq', self._temp(op2), '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_mul(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%rax'); self._emit('imulq', self._temp(op2)); self._emit('movq', '%rax', self._temp(dst))
    def _emit_div(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%rax'); self._emit('cqto'); self._emit('idivq', self._temp(op2)); self._emit('movq', '%rax', self._temp(dst))
    def _emit_mod(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%rax'); self._emit('cqto'); self._emit('idivq', self._temp(op2)); self._emit('movq', '%rdx', self._temp(dst))
    def _emit_neg(self, src, dst): self._emit('movq', self._temp(src), '%r11'); self._emit('negq', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_not(self, src, dst): self._emit('movq', self._temp(src), '%r11'); self._emit('notq', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_and(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('andq', self._temp(op2), '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_or(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('orq', self._temp(op2), '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_xor(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('xorq', self._temp(op2), '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_shl(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('movq', self._temp(op2), '%rcx'); self._emit('salq', '%cl', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_shr(self, op1, op2, dst): self._emit('movq', self._temp(op1), '%r11'); self._emit('movq', self._temp(op2), '%rcx'); self._emit('sarq', '%cl', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    
    def _emit_jmp(self, lbl): self._emit('jmp', lbl)
    def _emit_jz(self, op, lbl): self._emit('cmpq', '$0', self._temp(op)); self._emit('jz', lbl)
    def _emit_jnz(self, op, lbl): self._emit('cmpq', '$0', self._temp(op)); self._emit('jnz', lbl)
    def _emit_jlt(self, op, lbl): self._emit('cmpq', '$0', self._temp(op)); self._emit('jl', lbl)
    def _emit_jle(self, op, lbl): self._emit('cmpq', '$0', self._temp(op)); self._emit('jle', lbl)
    def _emit_jgt(self, op, lbl): self._emit('cmpq', '$0', self._temp(op)); self._emit('jg', lbl)
    def _emit_jge(self, op, lbl): self._emit('cmpq', '$0', self._temp(op)); self._emit('jge', lbl)

    def _emit_param(self, arg): self._params.append(arg)
    def _emit_param_slink(self, slink): self._pending_slink = slink

    def _setup_call(self, nargs):
        for i, x in enumerate(self._params[:6]):
            if isinstance(x, str) and x.startswith('@'):
                self._emit('movq', self._temp(x), self.PARAMS[i])
            else:
                self._emit('movq', self._temp(x), self.PARAMS[i])
        stack_args = self._params[6:][::-1]
        for x in stack_args:
            if isinstance(x, str) and x.startswith('@'):
                self._emit('movq', self._temp(x), '%r11')
                self._emit('pushq', '%r11')
            else:
                self._emit('pushq', self._temp(x))
        slink = self._pending_slink
        if isinstance(slink, int): self._emit('movq', f'${slink}', '%r11'); self._emit('pushq', '%r11')
        else: self._emit('pushq', self._temp(slink))
        self._emit('pushq', '$0')
        return (len(stack_args) + 2) * 8

    def _emit_call(self, lbl, nargs, ret=None):
        cleanup = self._setup_call(nargs)
        self._emit('callq', lbl)
        if cleanup: self._emit('addq', f'${cleanup}', '%rsp')
        if ret: self._emit('movq', '%rax', self._temp(ret))
        self._params = []; self._pending_slink = 0

    def _emit_icall(self, code_ptr, nargs, ret=None):
        cleanup = self._setup_call(nargs)
        self._emit('movq', self._temp(code_ptr), '%r10')
        self._emit('callq', '*%r10')
        if cleanup: self._emit('addq', f'${cleanup}', '%rsp')
        if ret: self._emit('movq', '%rax', self._temp(ret))
        self._params = []; self._pending_slink = 0

    def _emit_ret(self, ret=None):
        if ret: self._emit('movq', self._temp(ret), '%rax')
        self._emit('jmp', self._endlbl)

    def _emit_alloc_closure(self, dst): self._emit('subq', '$16', '%rsp'); self._emit('movq', '%rsp', self._temp(dst))
    def _emit_frameptr(self, dst): self._emit('movq', '%rbp', self._temp(dst))
    
    def _emit_store_code_ptr(self, label, base):
        # RIP-relative addressing just in case its ran w PIC
        self._emit('leaq', f'{label}(%rip)', '%r11')
        self._emit('movq', self._temp(base), '%r10')
        self._emit('movq', '%r11', '0(%r10)')
        
    def _emit_load_slink(self, dst): self._emit('movq', '24(%rbp)', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    
    def _emit_load_prev_slink(self, base, dst):
        self._emit('movq', self._temp(base), '%r10')
        self._emit('movq', '24(%r10)', '%r11') 
        self._emit('movq', '%r11', self._temp(dst))

    def _emit_load_mem(self, base, offset, dst):
        self._emit('movq', self._temp(base), '%r10'); self._emit('movq', f'{offset}(%r10)', '%r11'); self._emit('movq', '%r11', self._temp(dst))
    def _emit_store_mem(self, src, base, offset):
        self._emit('movq', self._temp(base), '%r10'); self._emit('movq', self._temp(src), '%r11'); self._emit('movq', '%r11', f'{offset}(%r10)')

    @classmethod
    def lower1(cls, tac: TACProc | TACVar) -> list[str]:
        emitter = cls()
        match tac:
            case TACVar(name, init):
                emitter._emit('.data'); emitter._emit('.globl', name); emitter._emit_label(name); emitter._emit('.quad', str(init))
                return emitter._asm
            case TACProc(name, arguments, ptac):
                emitter._endlbl = f'.E_{name}'
                for i in range(min(6, len(arguments))): emitter._emit('movq', emitter.PARAMS[i], emitter._temp(arguments[i]))
                for i, arg in enumerate(arguments[6:]): emitter._tparams[arg] = i
                for instr in ptac: emitter(instr)
                nvars = (max(emitter._temps.values()) + 1) if emitter._temps else 0
                nvars += nvars & 1
                return [
                    emitter._get_asm('.text'), emitter._get_asm('.globl', name), emitter._get_label(name),
                    emitter._get_asm('pushq', '%rbp'), emitter._get_asm('movq', '%rsp', '%rbp'),
                    emitter._get_asm('subq', f'${8*nvars}', '%rsp')
                ] + emitter._asm + [
                    emitter._get_label(emitter._endlbl), emitter._get_asm('movq', '%rbp', '%rsp'),
                    emitter._get_asm('popq', '%rbp'), emitter._get_asm('retq')
                ]

AsmGen.register(AsmGen_x64_Linux)


# ====================================================================
# ARM64 macOS Backend
# ====================================================================
class AsmGen_arm64_Darwin(AsmGen):
    NAME    = 'arm64-apple-darwin'
    SYSTEM  = 'Darwin'
    MACHINE = 'arm64'
    PARAMS  = [f'X{i}' for i in range(8)]

    def __init__(self):
        super().__init__()
        self._params = []
        self._endlbl = None

    def _format_temp(self, index):
        if isinstance(index, str):
            return [('adrp', 'X15', f'_{index}@PAGE')], f'[X15, _{index}@PAGEOFF]'
        off = 8*(index+1)
        if off > 256:
            return [('sub', 'X15', 'FP', f'#{off}')], '[X15]'
        return [], f'[FP, #-{off}]'

    def _format_param(self, index):
        return f'[FP, #{16 + 8*index}]'

    def _emit_const(self, ctt, dst):
        if ctt < 0: ctt = (1 << 64) + ctt
        self._emit('movz', 'X9', f'#{ctt & 0xffff}')
        ctt >>= 16; shift = 16
        while ctt > 0:
            if (ctt & 0xffff) != 0: self._emit('movk', 'X9', f'#{ctt & 0xffff}', f'lsl {shift}')
            ctt >>= 16; shift += 16
        self._emit('str', 'X9', self._temp(dst))

    def _emit_copy(self, src, dst): self._emit('ldr', 'X9', self._temp(src)); self._emit('str', 'X9', self._temp(dst))
    def _emit_add(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('add', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_sub(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('sub', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_mul(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('mul', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_div(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('sdiv', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_mod(self, op1, op2, dst): 
        self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2))
        self._emit('sdiv', 'X11', 'X9', 'X10'); self._emit('msub', 'X11', 'X11', 'X10', 'X9')
        self._emit('str', 'X11', self._temp(dst))
    
    def _emit_neg(self, src, dst): self._emit('ldr', 'X9', self._temp(src)); self._emit('neg', 'X9', 'X9'); self._emit('str', 'X9', self._temp(dst))
    def _emit_not(self, src, dst): self._emit('ldr', 'X9', self._temp(src)); self._emit('mvn', 'X9', 'X9'); self._emit('str', 'X9', self._temp(dst))
    def _emit_and(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('and', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_or(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('orr', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_xor(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('eor', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_shl(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('lsl', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))
    def _emit_shr(self, op1, op2, dst): self._emit('ldr', 'X9', self._temp(op1)); self._emit('ldr', 'X10', self._temp(op2)); self._emit('asr', 'X11', 'X9', 'X10'); self._emit('str', 'X11', self._temp(dst))

    def _emit_jmp(self, lbl): self._emit('b', lbl)
    def _emit_jz(self, op, lbl): self._emit('ldr', 'X9', self._temp(op)); self._emit('cbz', 'X9', lbl)
    def _emit_jnz(self, op, lbl): self._emit('ldr', 'X9', self._temp(op)); self._emit('cbnz', 'X9', lbl)
    def _emit_jlt(self, op, lbl): self._emit('ldr', 'X9', self._temp(op)); self._emit('cmp', 'X9', '#0'); self._emit('b.lt', lbl)
    def _emit_jle(self, op, lbl): self._emit('ldr', 'X9', self._temp(op)); self._emit('cmp', 'X9', '#0'); self._emit('b.le', lbl)
    def _emit_jgt(self, op, lbl): self._emit('ldr', 'X9', self._temp(op)); self._emit('cmp', 'X9', '#0'); self._emit('b.gt', lbl)
    def _emit_jge(self, op, lbl): self._emit('ldr', 'X9', self._temp(op)); self._emit('cmp', 'X9', '#0'); self._emit('b.ge', lbl)

    def _emit_param(self, arg): self._params.append(arg)
    def _emit_param_slink(self, slink): self._pending_slink = slink

    def _setup_call(self, nargs):
        for i, x in enumerate(self._params[:8]):
            if isinstance(x, str) and x.startswith('@'):
                self._emit('ldr', self.PARAMS[i], self._temp(x))
            else:
                self._emit('ldr', self.PARAMS[i], self._temp(x))
        stack_args = self._params[8:][::-1]
        count = len(stack_args) + 2
        if count % 2 != 0: count += 1 
        self._emit('sub', 'SP', 'SP', f'#{count*8}')
        self._emit('str', 'XZR', f'[SP, #{(len(stack_args)+1)*8}]') 
        sl_val = self._pending_slink
        if isinstance(sl_val, int): self._emit('mov', 'X9', f'#{sl_val}'); self._emit('str', 'X9', f'[SP, #{len(stack_args)*8}]')
        else: self._emit('ldr', 'X9', self._temp(sl_val)); self._emit('str', 'X9', f'[SP, #{len(stack_args)*8}]')
        for i, x in enumerate(stack_args):
            off = (len(stack_args) - 1 - i) * 8
            if isinstance(x, str) and x.startswith('@'): self._emit('ldr', 'X9', self._temp(x))
            else: self._emit('ldr', 'X9', self._temp(x))
            self._emit('str', 'X9', f'[SP, #{off}]')
        return count * 8

    def _emit_call(self, lbl, nargs, ret=None):
        cleanup = self._setup_call(nargs)
        self._emit('bl', '_' + lbl)
        if cleanup: self._emit('add', 'SP', 'SP', f'#{cleanup}')
        if ret: self._emit('str', 'X0', self._temp(ret))
        self._params = []; self._pending_slink = 0

    def _emit_icall(self, code_ptr, nargs, ret=None):
        cleanup = self._setup_call(nargs)
        self._emit('ldr', 'X9', self._temp(code_ptr))
        self._emit('blr', 'X9')
        if cleanup: self._emit('add', 'SP', 'SP', f'#{cleanup}')
        if ret: self._emit('str', 'X0', self._temp(ret))
        self._params = []; self._pending_slink = 0

    def _emit_ret(self, ret=None):
        if ret: self._emit('ldr', 'X0', self._temp(ret))
        self._emit('b', self._endlbl)

    def _emit_alloc_closure(self, dst): self._emit('sub', 'SP', 'SP', '#16'); self._emit('mov', 'X9', 'SP'); self._emit('str', 'X9', self._temp(dst))
    def _emit_store_code_ptr(self, label, base):
        name = '_' + label[1:]
        self._emit('adrp', 'X9', f'{name}@PAGE'); self._emit('add', 'X9', 'X9', f'{name}@PAGEOFF')
        self._emit('ldr', 'X10', self._temp(base)); self._emit('str', 'X9', '[X10, #0]')
    def _emit_frameptr(self, dst): self._emit('mov', 'X9', 'FP'); self._emit('str', 'X9', self._temp(dst))
    def _emit_load_slink(self, dst): self._emit('ldr', 'X9', '[FP, #16]'); self._emit('str', 'X9', self._temp(dst))
    

    def _emit_load_prev_slink(self, base, dst):
        self._emit('ldr', 'X10', self._temp(base))
        self._emit('ldr', 'X11', '[X10, #16]') 
        self._emit('str', 'X11', self._temp(dst))

    def _emit_load_mem(self, base, offset, dst):
        self._emit('ldr', 'X10', self._temp(base)); self._emit('ldr', 'X11', f'[X10, #{offset}]'); self._emit('str', 'X11', self._temp(dst))
    def _emit_store_mem(self, src, base, offset):
        self._emit('ldr', 'X10', self._temp(base)); self._emit('ldr', 'X11', self._temp(src)); self._emit('str', 'X11', f'[X10, #{offset}]')

    @classmethod
    def lower1(cls, tac: TACProc | TACVar) -> list[str]:
        emitter = cls()
        match tac:
            case TACVar(name, init):
                emitter._emit('.data'); emitter._emit('.globl', '_'+name); emitter._emit_label('_'+name); emitter._emit('.quad', str(init))
                return emitter._asm
            case TACProc(name, arguments, ptac):
                emitter._endlbl = f'.E_{name}'
                for i in range(min(8, len(arguments))): emitter._emit('str', emitter.PARAMS[i], emitter._temp(arguments[i]))
                for i, arg in enumerate(arguments[8:]): emitter._tparams[arg] = i
                for instr in ptac: emitter(instr)
                nvars = (max(emitter._temps.values()) + 1) if emitter._temps else 0
                nvars += nvars & 1
                return [
                    emitter._get_asm('.text'), emitter._get_asm('.globl', '_'+name), emitter._get_label('_'+name),
                    emitter._get_asm('stp', 'FP', 'LR', '[SP, #-16]!'), emitter._get_asm('mov', 'FP', 'SP'),
                    emitter._get_asm('sub', 'SP', 'SP', f'#{8*nvars}')
                ] + emitter._asm + [
                    emitter._get_label(emitter._endlbl), emitter._get_asm('mov', 'SP', 'FP'),
                    emitter._get_asm('ldp', 'FP', 'LR', '[SP]', '#16'), emitter._get_asm('ret')
                ]

AsmGen.register(AsmGen_arm64_Darwin)