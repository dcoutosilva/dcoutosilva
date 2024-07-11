import pyautogui
import time
from random import *
import datetime
import os
senha_ava = "0027987aA@@"

def seleciona_arquivo_rstudio():
    time.sleep(5)
    pyautogui.click(x=300, y=742)
    time.sleep(5)
    pyautogui.press("s")
    time.sleep(5)
    pyautogui.press("enter")
    time.sleep(5)

def relatorios():
    try:
        pyautogui.click(x=473, y=170)
        time.sleep(10)

        conclusao = pyautogui.locateCenterOnScreen("conclusao.png", confidence=0.8)
        if conclusao is not None:
            pyautogui.click(conclusao)
        else:
            print("Imagem 'conclusao.png' não encontrada")
            return

        time.sleep(10)
        pyautogui.click(x=346, y=550)
        time.sleep(10)
        pyautogui.scroll(-200)
        time.sleep(10)
        pyautogui.click(x=585, y=485)
        time.sleep(5)

        pasta_relatorio = pyautogui.locateCenterOnScreen("relatorio_pasta.png", confidence=0.8)
        if pasta_relatorio is not None:
            pyautogui.click(pasta_relatorio)
        else:
            print("Imagem 'relatorio_pasta.png' não encontrada")
            return

        time.sleep(3)

        bimestre2 = pyautogui.locateCenterOnScreen("2Bimestre.png", confidence=0.8)
        if bimestre2 is not None:
            pyautogui.doubleClick(bimestre2)
        else:
            print("Imagem '2Bimestre.png' não encontrada")
            return

        time.sleep(3)

        hoje = str(datetime.date.today())
        pasta = f"/home/danilo/Downloads/Amilcare/tecnico/Relatórios/2Bimestre/{hoje}"
        if not os.path.exists(pasta):
            np = pyautogui.locateCenterOnScreen("nova_pasta.png", confidence=0.8)
            if np is not None:
                pyautogui.click(np)
                time.sleep(2)
                pyautogui.write(hoje)
                time.sleep(1)
                pyautogui.press("enter")
            else:
                print("Imagem 'nova_pasta.png' não encontrada")
                return
        else:
            pyautogui.doubleClick(x=438, y=341)

        time.sleep(1)

        salvar = pyautogui.locateCenterOnScreen("salvar.png", confidence=0.8)
        if salvar is not None:
            pyautogui.click(salvar)
        else:
            print("Imagem 'salvar.png' não encontrada")
            return

        time.sleep(1)

        minhas_disciplinas = pyautogui.locateCenterOnScreen("minhas_disciplinas.png", confidence=0.8)
        if minhas_disciplinas is not None:
            pyautogui.click(minhas_disciplinas)
        else:
            print("Imagem 'minhas_disciplinas.png' não encontrada")
            return

    except pyautogui.ImageNotFoundException as e:
        print(f"Imagem não encontrada: {e}")
    except Exception as e:
        print(f"Ocorreu um erro: {e}")


pyautogui.pause = 0.3

pyautogui.press("win")
time.sleep(5)
pyautogui.write("chrome")
pyautogui.press("enter")
time.sleep(5)
#pyautogui.moveTo(x=744, y=767)
#time.sleep(5)
#pyautogui.click(x=744, y=767)

pyautogui.hotkey("ctrl", "t")
time.sleep(5)

#pyautogui.pause = 1
pyautogui.write("https://educacaoprofissional.educacao.sp.gov.br/my/courses.php", interval=0.25)
pyautogui.press("enter")
time.sleep(10)

#pyautogui.click(x=376, y=696)
#
#pyautogui.press("tab")

#pyautogui.hold(-200)
#pyautogui.click(x=506, y=749)
try:
    email = pyautogui.locateCenterOnScreen("email.png", confidence=0.8)
    pyautogui.click(email)
    pyautogui.write("rg487978833sp")
    #senha = pyautogui.locateCenterOnScreen("senha.png", confidence=0.8)
    #pyautogui.click(senha)
    pyautogui.press("tab")
    pyautogui.write(senha_ava)
    pyautogui.press("enter")
    time.sleep(5)
except:
    print(f"Usuário Logado")


#acessar = pyautogui.locateAllOnScreen("acessar.png")

pyautogui.click(x=373, y=364)#botão carreiras
time.sleep(5)
relatorios()
time.sleep(5)

pyautogui.click(x=744, y=367)#botão IA
time.sleep(5)
relatorios()
time.sleep(5)

pyautogui.click(x=1173, y=368)#botão logica
time.sleep(5)
relatorios()
time.sleep(5)

pyautogui.click(x=374, y=529)#metodologias ageis
time.sleep(5)
relatorios()
time.sleep(5)

pyautogui.click(x=777, y=540)#redes
time.sleep(5)
relatorios()
time.sleep(5)

pyautogui.click(x=1173, y=543)#versionamento
time.sleep(5)
relatorios()
time.sleep(5)

pyautogui.press("win")
time.sleep(5)
pyautogui.write("rstudio")
time.sleep(2)
pyautogui.click(x=692, y=199)
time.sleep(60)

#pyautogui.click(x=342, y=338)
#rscript = pyautogui.locateCenterOnScreen("rscript.png", confidence=0.8)
#pyautogui.click(rscript)
#pyautogui.hotkey("ctrl", "a")
pyautogui.click(x=329, y=344)#clicar nas linhas do codigo
time.sleep(1)
pyautogui.click(x=55, y=75)#menu edit
time.sleep(1)
pyautogui.click(x=111, y=328)#ação ctrl+A
time.sleep(1)
#pyautogui.hotkey("ctrl", "enter")
pyautogui.click(x=539, y=152)#ctrl+enter
time.sleep(10)
pyautogui.press("esc")


pyautogui.click(x=539, y=152)#ctrl+enter
time.sleep(5)
pyautogui.doubleClick(x=638, y=333)#primeiro arquivo
seleciona_arquivo_rstudio()


pyautogui.doubleClick(x=643, y=359)#segundo arquivo
seleciona_arquivo_rstudio()

pyautogui.doubleClick(x=565, y=374)#terceiro arquivo
seleciona_arquivo_rstudio()



pyautogui.doubleClick(x=629, y=402)#quarto arquivo
seleciona_arquivo_rstudio()



pyautogui.doubleClick(x=560, y=427)#quinto arquivo
seleciona_arquivo_rstudio()


pyautogui.doubleClick(x=595, y=447)#sexto arquivo
time.sleep(5)
pyautogui.click(x=300, y=742)
time.sleep(5)
pyautogui.press("n")
time.sleep(5)
pyautogui.press("enter")
time.sleep(5)


