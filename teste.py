'''
Crie um programa em Python que solicite a idade do usuário e informe se ele possui idade
suficiente para assistir a um filme.
Caso a idade seja maior ou igual a 18, o programa deve exibir a mensagem: "Você pode
assistir ao filme.". Caso contrário, a mensagem deve ser: "Desculpe, você não tem idade
suficiente para assistir ao filme.".
'''
while True:
    try:
        from colorama import Fore, Style, init
        init()

        idade = int(input("Digite sua idade: "))

        if idade >= 18:
            print(Fore.GREEN + "Você pode assistir ao filme.")
        else:
            print(Fore.RED + "Desculpe, você não tem idade suficiente para assistir ao filme.")
        break
    except:
        print("Digite apenas números para sua idade.")
        continue
