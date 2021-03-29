**************
* Instalação *
**************

O programa foi desenvolvido em linguagem OCaml com a versão 4.11.1 e
testado no sistema operativo Fedora 33 (linux)

O método de instalação utilizado é o descrito na página oficial do Ocaml
( https://ocaml.org/docs/install.html ) para a versão de SO referida anteriormente

$  dnf install ocaml

**************
* Compilação *
**************

Para compilar o programa basta escrever na linha de comandos o seguinte comando:

$  ocamlopt -o snsidoso snsidoso.ml

**************
*  Execução  *
**************

Para executar o programa basta escrever na linha de comandos o seguinte comando:

$  ./snsidoso

Após o programa iniciar será solicitádo o nome do ficheiro que deverá ser lido.
O ficheiro deverá estar estruturado da mesma forma que o ficheiro de exemplo "dados.txt"