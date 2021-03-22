(*Estrutura de Dados de um Registo*)
type registiIdoso ={
  nome : string;
  ndoenca : int;
  medicamento : int;
  acidente : bool;
  doenca : bool;
  sozinho : bool;
  autonomia : bool;
  desportoj : bool;
  autofisica : bool;
  fisica : bool;
  profissaorisco : string;
}

(*Função para ler o ficheiro
  * Percorre cada linha e adiciona a linha numa lista
  * retorna a lista de linhas (lista de strings)
*)
let lerFicheiro ficheiro: string list =(
  let ler = open_in ficheiro in                   (*Abrir o ficheiro*)
  let try_read () =                               
    try                                           (*Verificar: *)
      Some (input_line ler)                       (*Leu algo *)
    with End_of_file -> None in                   (*Chegou ao fim do ficheiro *)
    let rec lerLinha lista =                      (*Função que lê a linha recursivamente *)
      match try_read() with                       (*Verifica: *)
      | Some linha -> lerLinha (linha::lista)     (*-Leu linha*)
      | None -> close_in ler;                     (*-Não leu nada e fecha o ficheiro*)
      List.rev lista in                           (*Inverte os elementos da lista*)
        lerLinha []                               (*Chama a função de ler linhas*)
);;

(* Ler os dados do ficheiro *)
let dadosLista = lerFicheiro "dados.txt";;

let separarDados linha =(
    String.split_on_char ';' linha
);;
let rec organizarDados dados = (
    match dados with
    | [] -> []
    | h::t -> (separarDados h)::
    organizarDados (List.tl dados)
);;

(organizarDados dadosLista);;
