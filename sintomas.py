sintoma1 = input("O paciente tem febre? (sim/não): ")
sintoma2 = input("O paciente tem tosse? (sim/não): ")

if sintoma1 == "sim" and sintoma2 == "sim":
    print("Possível diagnóstico: Gripe")
elif sintoma1 == "sim" and sintoma2 == "não":
    print("Possível diagnóstico: Infecção viral")
else:
    print("Sintomas não específicos, consulte um médico")
