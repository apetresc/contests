def extract_param(M, ip, p, pmodes):
    if p > len(pmodes):
        mode = 0
    else:
        mode = pmodes[-p]

    if mode == 0:
        return M[M[ip + p]]
    elif mode == 1:
        return M[ip + p]


def exec_instruction(M, ip):
    reg = M[ip]
    op = reg % 100
    pmodes = []

    reg //= 100
    while reg:
        pmodes.insert(0, reg % 10)
        reg //= 10

    if op == 1:    # ADD
        M[M[ip + 3]] = extract_param(M, ip, 1, pmodes) + extract_param(M, ip, 2, pmodes)
        return ip + 4
    elif op == 2:  # MULT
        M[M[ip + 3]] = extract_param(M, ip, 1, pmodes) * extract_param(M, ip, 2, pmodes)
        return ip + 4
    elif op == 3:  # READ
        M[M[ip + 1]] = int(input())
        return ip + 2
    elif op == 4:  # WRITE
        print(extract_param(M, ip, 1, pmodes))
        return ip + 2
    elif op == 5:  # JIT
        if extract_param(M, ip, 1, pmodes):
            return extract_param(M, ip, 2, pmodes)
        else:
            return ip + 3
    elif op == 6:  # JIF
        if not extract_param(M, ip, 1, pmodes):
            return extract_param(M, ip, 2, pmodes)
        else:
            return ip + 3
    elif op == 7:  # LE
        if extract_param(M, ip, 1, pmodes) < extract_param(M, ip, 2, pmodes):
            M[M[ip + 3]] = 1
        else:
            M[M[ip + 3]] = 0
        return ip + 4
    elif op == 8:  # EQ
        if extract_param(M, ip, 1, pmodes) == extract_param(M, ip, 2, pmodes):
            M[M[ip + 3]] = 1
        else:
            M[M[ip + 3]] = 0
        return ip + 4
    elif op == 99: # RET
        return -1
    else:
        raise ValueError("Invalid opcode %d at position %d" % (M[ip], ip))

def run_program(M):
    ip = 0
    while True:
        ip = exec_instruction(M, ip)
        if ip < 0:
            return

if __name__ == "__main__":
    M = list(map(int, input().split(",")))
    run_program(M)
    print("Final memory: %s" % M)
