def paques (A):
    R=A%4
    S=A%7
    T=A%19
    B=19*T+24
    M=B%30
    C=2*R+4*S+6*M+5
    N=C%7
    P=M+N
    if P<=9:
        print(P+22)
    else:
        print(P-9)


paques(2023)