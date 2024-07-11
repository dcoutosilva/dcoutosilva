dias = int(input("Digite a quantidade de dias: "))

anos = dias // 365
meses = (dias % 365) // 30
dias_restantes = (dias % 365) % 30

print(f"{anos} anos, {meses} meses e {dias_restantes} dias")

dias_recalculado = dias_restantes + (meses * 30) + (anos * 365)

print(f'os dias recalculados são {dias_recalculado}')