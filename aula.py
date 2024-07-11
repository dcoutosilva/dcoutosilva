"""4) Crie um programa que exiba uma tabela de multiplicação para um número fornecido pelo usuário. Utilize o laço
for para iterar de 1 a 10 e imprimir o produto do número fornecido com cada um desses valores.
Orientações:
· Peça ao usuário para inserir um número;
· Use um laço for para percorrer um intervalo de 1 a 10;
· Em cada iteração, calcule o produto do número fornecido pelo valor atual do laço;
· Imprima cada produto em uma nova linha.

"""

numero = int(input("Qual o numero desejado: "))

for i in range(11):
  print(f"A multiplicacao de {numero} por {i} é igual {numero * i}")