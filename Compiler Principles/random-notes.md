# Compilers

Semanticka pravidla v Cpp

Mnoziny definovane strukturou a tu pridat k terminalum "k tomu `%token`"

Neterminaly - `%type<T>____`

```
%type<s> B
%type<T>______


$   1 2 3 4 5
A : B C D E F; {
}
$$.a = $1.a + $3.a

Pozice
@a @@

```

Cpp kod v pravidlech ma vlastni cislo
```
$   1       2     3 4 5        6
A : B[name] {cpp} C D E[name2] F; {
auto variable = name;
}
```

`ctx->[tabulka]`
rozlisovat lokalni a globablni promenne

info v tabulce:
```
|index|identifikator typu|pointer na typ|
```
typ - napr `int`


tabulka typu obsahuje i:
```
"box s navaznosti typu"

type r = recotd
  i: int
  f: real
```


tabulka labelu
AddLabelEntry
```
|cislo|adresa na skok|
```

VOLAT funkci enter a leave u funkci kdyz se leze do bloku

existuje fce `IsInsideFunctionBlock`
dalsi `MyFunctionName` (vhodna na check if return is in correct function)

// dalsi `MyReturnAddress` ale ta az na dalsi ulohy


Info k pridavani do tabulek je v dokumentaci.

Hlavne pozor na check type pri pridavani do tabulky symbolu
`tr->type` je odkaz z symbolu do typu