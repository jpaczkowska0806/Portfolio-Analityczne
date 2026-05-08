#zadanie 1
def to_onp(wyrazenie):

    stos=[]
    wynik=''
    
    for znak in wyrazenie:
        if znak.isdigit():
            wynik+=znak
        elif znak=='(':
            stos.append(znak)
        elif znak==')':
            if '(' not in stos:
                raise ValueError("W podanym wyrażeniu brakuje nawiasu otwierającego.")
            while stos and stos[-1]!='(':
                wynik+=stos.pop()
            stos.pop()
        elif znak=='+':
            while stos and stos[-1]!='(':
                wynik+=stos.pop()
            stos.append('+')
        elif znak=='*':
            while stos and stos[-1]=='*':
                wynik+=stos.pop()
            stos.append('*')
        else:
            raise ValueError(f"Funkcja nie rozpoznaje takiego znaku: '{znak}'.")

    while stos:
        top=stos.pop()
        if top=='(':
            raise ValueError("W podanym wyrażeniu brakuje nawiasu zamykającego.")
        wynik+=top

    return wynik

#zadanie 2
def eval_onp(wyrazenie):

    stos=[]
    operatory=['+','*']
    
    for znak in wyrazenie:
        if znak.isdigit():
            stos.append(int(znak))
        elif znak in operatory:
            if len(stos)<2:
                raise ValueError("Na stosie jest za mało liczb.")
            b=stos.pop()
            a=stos.pop()
            if znak=='+':
                wynik=(a+b)%10
            else:
                wynik=(a*b)%10
            stos.append(wynik)
        else:
            raise ValueError(f"Funkcja nie rozpoznaje takiego znaku: '{znak}'.")
    
    if len(stos)!=1:
        raise ValueError("Wprowadzone wyrażenie jest niepoprawne.")

    return stos.pop()
        
#zadanie 3
def eval_m10(wyrazenie):
    onp=to_onp(wyrazenie)
    wynik=eval_onp(onp)
    return wynik
    
#zadanie 6
def main():
    while True:
        wyrazenie=input("Podaj wyrażenie w notacji infiksowej lub wpisz 'quit', aby zakończyć: ")
        if wyrazenie=='quit':
            print("Koniec działania programu.")
            break
        wynik=eval_m10(wyrazenie)
        print(f"Wartość Twojego wyrażenia to {wynik}.")

if __name__ == "__main__":
    main()

        
