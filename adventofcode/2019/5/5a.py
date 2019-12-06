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
        return 4
    elif op == 2:  # MULT
        M[M[ip + 3]] = extract_param(M, ip, 1, pmodes) * extract_param(M, ip, 2, pmodes)
        return 4
    elif op == 3:  # READ
        M[M[ip + 1]] = int(input())
        return 2
    elif op == 4:  # WRITE
        print(extract_param(M, ip, 1, pmodes))
        return 2
    elif op == 99: # RET
        return -1
    else:
        raise ValueError("Invalid opcode %d at position %d" % (M[ip], ip))

def run_program(M):
    ip = 0
    n = 0
    while True:
        n += 1
        offset = exec_instruction(M, ip)
        if offset >= 0:
            ip += offset
        else:
            return n

if __name__ == "__main__":
    M = list(map(int, input().split(",")))
    run_program(M)
    print("Final memory: %s" % M)
