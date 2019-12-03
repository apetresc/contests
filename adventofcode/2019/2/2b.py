def exec_instruction(M, ip):
    if M[ip] == 1:
        M[M[ip + 3]] = M[M[ip + 1]] + M[M[ip + 2]]
        return 4
    elif M[ip] == 2:
        M[M[ip + 3]] = M[M[ip + 1]] * M[M[ip + 2]]
        return 4
    elif M[ip] == 99:
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

    for i in range(100):
        for j in range(100):
            Ma = list(M)
            Ma[1:3] = i, j
            try:
                n = run_program(Ma)
            except ValueError:
                pass
            if Ma[0] == 19690720:
                print("SUCCESS WITH %d,%d" % (i,j))
                pass
            else:
                pass
