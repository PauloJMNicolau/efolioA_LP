(** 
  * Paulo Nicolau (UAB estudante 1800465) - 2021
  * Código sobre licença MIT
  * 
  * Programa que lê os dados de um ficheiro txt estruturado com dados sobre idosos
  * Carrega a informação e depois analisa os dados em memória 
  * Da análise o programa retorna o grau de risco de cada idoso
  * Guarda a informação num ficheiro txt
*)

(*Estrutura de Dados de um Registo*)
type registo ={
  mutable nome: string;
  mutable ndoenca : int;
  mutable medicamento : int;
  mutable acidente : bool;
  mutable doenca : bool;
  mutable sozinho : bool;
  mutable autonomia : bool;
  mutable desportoj : bool;
  mutable autofisica : bool;
  mutable fisica : bool;
  mutable profissaorisco : string;
}

(**********************
  * Converter Valores *
  *********************)

(* Converte a string recebida em valor booleano
   - Sim(s) em True 
   - Não(n) em false *)
let obterFactorBool valor =(
  if valor = "s" then
    true
  else 
    false 
);;

(*Obtém o valor em formato string, que se encontra na cabelça da lista recebida 
  - Retorna String do facto - caso receba algo
  - String vazia - caso lista vazia *)
let obterValorString lista = (
  match lista with
  | [] -> " "
  | h::t -> h
);;


(*Converte a String em valor Inteiro 
  Retorna o valor inteiro da string recebida *)
let obterValorInteiro valor = (
  int_of_string valor
);;


(***************************************
 * Funções para trabalhar com registos *
 **************************************)

(* Altera os valores no registo *)
let alteraRegisto reg valor tipo = (
  match tipo with
  | 0 -> (reg.nome <- valor;reg)
  | 1 -> (reg.ndoenca <- (obterValorInteiro valor);reg)
  | 2 -> (reg.medicamento <- (obterValorInteiro valor);reg)
  | 3 -> (reg.acidente <- (obterFactorBool valor); reg)
  | 4 -> (reg.doenca <- (obterFactorBool valor); reg)
  | 5 -> (reg.sozinho <- (obterFactorBool valor); reg)
  | 6 -> (reg.autonomia <- (obterFactorBool valor); reg)
  | 7 -> (reg.desportoj <- (obterFactorBool valor); reg)
  | 8 -> (reg.autofisica <- (obterFactorBool valor); reg)
  | 9 -> (reg.fisica <- (obterFactorBool valor); reg)
  | 10 -> (reg.profissaorisco <- valor; reg)
  | _ -> reg
);;

(* Preencher o registo com os valores recebidos *)
let preencherRegisto facto reg index =(
  let dados = (String.split_on_char ':' facto) in (               (* Separa o factor recebido pelo caracter ":" *)
  match dados with                                                (* Compara o dado com: *)
  | [] -> reg                                                     (* Lista vazia - Retorna o registo*)
  | h::t -> alteraRegisto reg (obterValorString t) (10-index)     (* Lista - Executa a alteração do registo *)
                                                                  (* Envia registo, string na cauda da lista *)
  )
);;



(*Separa os elementos da linha *)
let separar linha = (
  let reg:registo = {nome=" "; ndoenca=0; medicamento=0; acidente=false; doenca=false;      (* Cria um registo vazio *)
      sozinho=false; autonomia=false; desportoj=false; autofisica=false; fisica=false;
      profissaorisco=" "} in
  let factos = (String.split_on_char ';' linha) in                                          (* Separa os elementos da linah pelo ";" *)
  let rec  obter (factos, reg) = (                                                          (* Função Recursiva que percorre os elementos da linha *)
    match factos with                                                                       (* Compara o factor recebido *)
    | [] -> reg                                                                             (* Vazio - retorna o registo recebido *)
    | h::t ->                                                                               (* Lista de elementos: *)
        ignore (obter (t, reg));                                                            (* Executa a funçãor ecursivamente *) 
        (preencherRegisto h reg (List.length t))                                            (* Executa função que preenche o registo (envia o registo e o factor) *)
    )
  in obter (factos, reg);                                                                   (* Executa a função recursiva que percorre a lista*)
);;


(*Função para ler o ficheiro
  * Percorre cada linha e adiciona a linha numa lista
  * retorna a lista de linhas (lista de strings)
*)
let lerFicheiro ficheiro: registo list =(
  let ler = open_in ficheiro in                   (*Abrir o ficheiro*)
  let try_read () =                               
    try                                           (*Verificar: *)
      Some (input_line ler)                       (*Leu algo *)
    with End_of_file -> None in                   (*Chegou ao fim do ficheiro *)
    let rec lerLinha lista =                      (*Função que lê a linha recursivamente *)
      match try_read() with                       (*Verifica: *)
      | Some linha -> lerLinha ((separar linha)::lista)     (*-Leu linha*)
      | None -> close_in ler;                     (*-Não leu nada e fecha o ficheiro*)
      List.rev lista in                           (*Inverte os elementos da lista*)
        lerLinha []                               (*Chama a função de ler linhas*)
);;

(* Ler os dados do ficheiro *)
let dadosLista = lerFicheiro "dados.txt";;

(* Imprimir o conteudo do Registo *)
let imprimirRegisto reg = (
  ignore (print_string (reg.nome^" "));
  ignore (print_string ((string_of_int reg.ndoenca)^" "));
  ignore (print_string ((string_of_int reg.medicamento)^" "));
  ignore (print_string ((string_of_bool reg.doenca)^" "));
  ignore (print_string ((string_of_bool reg.acidente)^" "));
  ignore (print_string ((string_of_bool reg.sozinho)^" "));
  ignore (print_string ((string_of_bool reg.autonomia)^" "));
  ignore (print_string ((string_of_bool reg.desportoj)^" "));
  ignore (print_string ((string_of_bool reg.autofisica)^" "));
  ignore (print_string ((string_of_bool reg.fisica)^" "));
  ignore (print_endline reg.profissaorisco);
  
);;


let rec imprimir lista = (
    match lista with
    | [] -> []
    | h::t -> imprimirRegisto h; imprimir t;
);;
imprimir dadosLista;;
