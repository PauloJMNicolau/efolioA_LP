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
  match (String.compare "s" valor) with
  | 0 -> true
  | _ -> false 
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

(* Altera os valores no registo 
  - Executa opção de alteração recebida
  - Retorna Registo alterado *)
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
  | h::t -> alteraRegisto reg (obterValorString t) (10-index)     (* Lista - Executa a alteração do registo 
                                                                     Envia registo, string na cauda da lista e valor da opção a executar 
                                                                     (com base no indice da lista) *)
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
    | h::t ->(                                                                              (* Lista de elementos: *)
      let (_:registo) = (obter (t, reg)) in                                                 (* Executa a funçãor recursivamente *) 
        (preencherRegisto h reg (List.length t))                                            (* Executa função que preenche o registo (envia o registo e o factor) *)
      )
    )
  in obter (factos, reg);                                                                   (* Executa a função recursiva que percorre a lista*)
);;


(**************************************
 * Funções para a leitura do ficheiro *
 *************************************)

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

(**********************************
 * Funções para analise dos dados *
 *********************************)

 (* Retorna Lista de Valores Possiveis para o valor em nDoencas *)
 let analiseNDoencas valor=(
  match valor with
  | 0 | 1 -> (["Baixo"])
  | 2 -> (["Medio";"Baixo"])
  | _ -> (["Alto";"Baixo"])  
);;

(* Retorna Lista de Valores Possiveis para o valor em Medicamento *)
let analiseMedicamento valor=(
  match valor with
  | 0 | 1 | 2-> (["Medio";"Baixo"])
  | 3 -> (["Medio"])
  | _ -> (["Alto"])  
);;

(* Retorna Lista de Valores Possiveis para o valor em Acidentes *)
let analiseAcidentes valor =(
  match valor with
  |true -> (["Alto";"Medio"])
  |false -> (["Alto"; "Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em Doenca*)
let analiseDoenca valor =(
  match valor with
  |true -> (["Alto";"Medio"])
  |false -> (["Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em Sozinho*)
let analiseSozinho valor =(
  match valor with
  |true -> (["Alto";"Medio"])
  |false -> (["Alto"; "Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em Autonomia*)
let analiseAutonomia valor =(
  match valor with
  |true -> (["Medio";"Baixo"])
  |false -> (["Alto"; "Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em DesportoJ*)
let analiseDesportoj valor =(
  match valor with
  |true -> (["Alto";"Baixo"])
  |false -> (["Alto"; "Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em AutoFisica*)
let analiseAutofisica valor =(
  match valor with
  |true -> (["Medio";"Baixo"])
  |false -> (["Alto"; "Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em Fisica*)
let analiseFisica valor =(
  match valor with
  |true -> (["Alto";"Baixo"])
  |false -> (["Alto"; "Medio"; "Baixo"])
);;

(* Retorna Lista de Valores Possiveis para o valor em ProfissaoRisco*)
let analiseProfissaoRisco valor =(
  let tipos = ["alto";"medio";"baixo"] in
    let rec percorrer (x,indice) =
      match x with
      | [] -> []
      | h::t -> (
          match (String.compare valor h) with
          | 0 -> (
              match indice with
              | 0 -> (["Alto"])
              | 1 -> (["Alto";"Medio"])
              | 2 -> (["Medio"; "Baixo"])
              | _ -> ([])
            )
          | _ -> percorrer (t, indice+1);
      )
    in percorrer (tipos, 0)
);;

(*Verifica se valor x pertence à lista*)
let rec belongs x lista = (
  match lista with
  | [] -> false                         (*retorna false se chegar ao fim da lista ou for lista vazia*)
  | h::t -> 
    match (String.compare h x) with     (* Verifica se a string é igual *)
    | 0 -> true                         (* - Sim: retorna verdadeiro *)
    | _ -> belongs x t                  (* Não - Continua a percorrer a lista *)
);;

(*Retorna o conjunto interceção de ambos os elementos da lista*)
let rec inter lista1 lista2 =(
  match lista2 with
  | [] -> []                      (*Retorna a lista vazia no final*)
  | h::t ->
    match (belongs h lista1) with     (*Verifica se o elemento atual da lista2 está na lista1*)
    | true -> h::(inter lista1 t)     (*Cria uma nova lista com a cabeça em que a cauda será o resultado retornado da execução recursiva com a lista1 e a cauda da lista2*)
    | false -> (inter lista1 t)       (*Executa recursivamente com a lista1 e a cauda da lista2*)
);;

let rec analisaRegisto reg=(
  reg.nome::
    let rec comparar (cj, indice) =(
      match indice with
      | 0 -> comparar (inter cj (analiseNDoencas reg.ndoenca), (indice+1))
      | 1 -> comparar (inter cj (analiseMedicamento reg.medicamento), (indice+1))
      | 2 -> comparar (inter cj (analiseAcidentes reg.acidente), (indice+1))
      | 3 -> comparar (inter cj (analiseDoenca reg.doenca), (indice+1))
      | 4 -> comparar (inter cj (analiseSozinho reg.sozinho), (indice+1))
      | 5 -> comparar (inter cj (analiseAutonomia reg.autonomia), (indice+1))
      | 6 -> comparar (inter cj (analiseDesportoj reg.desportoj), (indice+1))
      | 7 -> comparar (inter cj (analiseAutofisica reg.autofisica), (indice+1))
      | 8 -> comparar (inter cj (analiseFisica reg.fisica), (indice+1))
      | 9 -> comparar (inter cj (analiseProfissaoRisco reg.profissaorisco), (indice+1))
      | _ -> cj
    ) in comparar (["Alto";"Medio";"Baixo"], 0);
);;

let rec imprimirList lista = (
    match lista with
    | [] -> print_endline(" ")
    | h::t -> print_string (h^" - "); imprimirList t;
);;
let rec analisar reg =(
  match reg with
  | [] -> []
  | h::t ->( 
    imprimirList(analisaRegisto h)::(analisar t)
  )
);;


(*******************************
 * Funções para imprimir dados *
 ******************************)

(* Imprimir o conteudo do Registo *)
let imprimirRegisto reg = (
  let (_:bool)= 
    (print_string (reg.nome^" "); 
    (print_string ((string_of_int reg.ndoenca)^" "));
    (print_string ((string_of_int reg.medicamento)^" "));
    (print_string ((string_of_bool reg.acidente)^" "));
    (print_string ((string_of_bool reg.doenca)^" "));
    (print_string ((string_of_bool reg.sozinho)^" "));
    (print_string ((string_of_bool reg.autonomia)^" ")); 
    (print_string ((string_of_bool reg.desportoj)^" "));
    (print_string ((string_of_bool reg.autofisica)^" ")); 
    (print_string ((string_of_bool reg.fisica)^" ")); 
    (print_endline reg.profissaorisco);true
    )in true
);;


let rec imprimir lista = (
    match lista with
    | [] -> []
    | h::t -> (let (_list) = imprimirRegisto h in imprimir t;)
);;

let rec imprimirList lista = (
    match lista with
    | [] -> []
    | h::t -> print_string (h^" "); imprimirList t;
);;

(************************
 * Execução do Programa *
 ***********************)

let dadosLista = lerFicheiro "dados.txt";;    (* Ler os dados do ficheiro *)
(*imprimir dadosLista;;                         (* Imprimir Valores *)
*)(analisar dadosLista);;