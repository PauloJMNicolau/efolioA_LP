(** 
  * Paulo Nicolau (UAB estudante 1800465) - 2021
  * Código sobre licença MIT
  * 
  * Programa que lê os dados de um ficheiro txt estruturado com dados sobre idosos
  * Carrega a informação e depois analisa os dados em memória 
  * Da análise o programa retorna o grau de risco de cada idoso
  * Guarda a informação num ficheiro txt e imprime no ecrã
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
      | Some linha -> lerLinha ((separar linha)::lista)     (*-Leu linha - separa os parametros da linha numa lista e adiciona essa lista como elemento numa outra lista*)
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
  _ when valor < 0 -> ([])
  | 0 | 1 -> (["Baixo"])                      (*Graus de risco para valores menores ou iguais a 1*)
  | 2 -> (["Medio";"Baixo"])                  (*Graus de risco para valorr 2*)
  | _ -> (["Alto";"Baixo"])                   (*Graus de risco para valores superiores a 2*)
);;

(* Retorna Lista de Valores Possiveis para o valor em Medicamento *)
let analiseMedicamento valor=(
  match valor with
  _ when valor < 0 -> ([])
  | 0 | 1 | 2-> (["Medio";"Baixo"])           (*Graus de risco para valores menores ou iguais a 2*)
  | 3 -> (["Medio"])                          (*Graus de risco para o valor 3*)
  | _ -> (["Alto"])                           (*Graus de risco para valores maiores que 3*)
);;

(* Retorna Lista de Valores Possiveis para o valor em Acidentes *)
let analiseAcidentes valor =(
  match valor with
  |true -> (["Alto";"Medio"])                 (*Graus de risco onde sim é aceite*)
  |false -> (["Alto"; "Medio"; "Baixo"])      (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em Doenca*)
let analiseDoenca valor =(
  match valor with
  |true -> (["Alto";"Medio"])                   (*Graus de risco onde sim é aceite*)
  |false -> (["Medio"; "Baixo"])                (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em Sozinho*)
let analiseSozinho valor =(
  match valor with
  |true -> (["Alto";"Medio"])                   (*Graus de risco onde sim é aceite*)
  |false -> (["Alto"; "Medio"; "Baixo"])        (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em Autonomia*)
let analiseAutonomia valor =(
  match valor with
  |true -> (["Medio";"Baixo"])                  (*Graus de risco onde sim é aceite*)
  |false -> (["Alto"; "Medio"; "Baixo"])        (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em DesportoJ*)
let analiseDesportoj valor =(
  match valor with
  |true -> (["Alto";"Baixo"])                   (*Graus de risco onde sim é aceite*)
  |false -> (["Alto"; "Medio"; "Baixo"])        (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em AutoFisica*)
let analiseAutofisica valor =(
  match valor with
  |true -> (["Medio";"Baixo"])                  (*Graus de risco onde sim é aceite*)
  |false -> (["Alto"; "Medio"; "Baixo"])        (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em Fisica*)
let analiseFisica valor =(
  match valor with
  |true -> (["Alto";"Baixo"])                   (*Graus de risco onde sim é aceite*)
  |false -> (["Alto"; "Medio"; "Baixo"])        (*Graus de risco onde não é aceite*)
);;

(* Retorna Lista de Valores Possiveis para o valor em ProfissaoRisco*)
let analiseProfissaoRisco valor =(
  let tipos = ["alto";"medio";"baixo"] in         (* Cria uma lista com os elementos a comparar *)
    let rec percorrer (x,indice) =                (* Percorre a lista de opções *)
      match x with
      | [] -> []
      | h::t -> (
          match (String.compare valor h) with     (* Verifica se o valor recebido é igual ao valor atual da lista*)
          | 0 -> (                                (* Se for verifica qual o elemento de comparação atual*)
              match indice with                   
              | 0 -> (["Alto"])                   (* Retorna a lista de valores possiveis quando ov alor recebido foi "alto"*)
              | 1 -> (["Alto";"Medio"])           (* Retorna a lista de valores possiveis quando ov alor recebido foi "medio"*)
              | 2 -> (["Baixo"])                  (* Retorna a lista de valores possiveis quando ov alor recebido foi "baixo"*)
              | _ -> ([])
            )
          | _ -> percorrer (t, indice+1);         (*Executa recursivamente o função que compara os elementos da lista com os valores *)
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
  | [] -> []                          (*Retorna a lista vazia no final*)
  | h::t ->
    match (belongs h lista1) with     (*Verifica se o elemento atual da lista2 está na lista1*)
    | true -> h::(inter lista1 t)     (*Cria uma nova lista com a cabeça em que a cauda será o resultado retornado da execução recursiva com a lista1 e a cauda da lista2*)
    | false -> (inter lista1 t)       (*Executa recursivamente com a lista1 e a cauda da lista2*)
);;

(*Analisa os valores que estão no registo*)
let rec analisaRegisto reg=(
  reg.nome::
    let rec comparar (cj, indice) =(    
      match indice with                                                                   (*Compara o valor do indice*)
      | 0 -> comparar (inter cj (analiseNDoencas reg.ndoenca), (indice+1))                (*Se for o primeiro elemento realiza interceção do conjunto inicial com o conjunto de ndoenca (correspondente ao valor no registo)*)
      | 1 -> comparar (inter cj (analiseMedicamento reg.medicamento), (indice+1))         (*Se for o segundo elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de medicamento (correspondente ao valor no registo)*)
      | 2 -> comparar (inter cj (analiseAcidentes reg.acidente), (indice+1))              (*Se for o terceiro elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de acidente (correspondente ao valor no registo)*)
      | 3 -> comparar (inter cj (analiseDoenca reg.doenca), (indice+1))                   (*Se for o quarto elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de doença (correspondente ao valor no registo)*)
      | 4 -> comparar (inter cj (analiseSozinho reg.sozinho), (indice+1))                 (*Se for o quinto elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de sozinho (correspondente ao valor no registo)*)
      | 5 -> comparar (inter cj (analiseAutonomia reg.autonomia), (indice+1))             (*Se for o sexto elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de autonomia (correspondente ao valor no registo)*)
      | 6 -> comparar (inter cj (analiseDesportoj reg.desportoj), (indice+1))             (*Se for o setimo elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de desportoj (correspondente ao valor no registo)*)
      | 7 -> comparar (inter cj (analiseAutofisica reg.autofisica), (indice+1))           (*Se for o oitavo elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de autofisica (correspondente ao valor no registo)*)
      | 8 -> comparar (inter cj (analiseFisica reg.fisica), (indice+1))                   (*Se for o nono elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de fisica (correspondente ao valor no registo)*)
      | 9 -> comparar (inter cj (analiseProfissaoRisco reg.profissaorisco), (indice+1))   (*Se for o decimo elemento realiza interceção do conjunto resultado da operação anterior com o conjunto de profissaorisco (correspondente ao valor no registo)*)
      | _ -> cj                                                                           (*No final devolve o conjunto vazio*)
    ) in comparar (["Alto";"Medio";"Baixo"], 0);                                          (*Executa recursivamente a função*)
);;

(*Executa as funções que analisam os dados *)
let rec analisar reg =(
  match reg with
  | [] -> []                            (*Caso receba uma lista vazia de registo devolve uma lista vazia*)
  | h::t ->(                            (*Caso receba uma lista de registos executa recursivamente a função, executando a função que analisa o registo*)
    (analisaRegisto h)::(analisar t)
  )
);;


(*******************************
 * Funções para imprimir dados *
 ******************************)

(*Gera as mensagens dos resultados*)
let rec gerarMensagens lista = (
    match lista with
    | [] -> ""
    | h::t -> ("O idoso "^h^
      let mensagemRisco risco =(
        match (String.compare risco " ") with
        | 0 -> " não possui risco"                    (*Mensagem quando não existe valor de risco*)
        | _ -> " possui risco "^risco                 (*Mensagem de quando existe valor de risco*)
      )in mensagemRisco (obterValorString t))
);;

(*Cria lista com as mensagens de resultados*)
let rec resultados lista = (
  match lista with
  | [] -> []
  | h::t -> gerarMensagens h::resultados t
);;

(*Imprimir mensagens de resultados*)
let rec imprimirResultado lista = (
  match lista with
  | [] -> "Não existem mais dados analisados!"
  | h::t -> print_endline h; imprimirResultado t
);;

let gravarResultado ficheiro resultado=(
let oc = open_out ficheiro in                               (* Abre o ficheiro para escrita *)
  let rec escrever lista =(                                 (* Percorre a lista de resultados*)
    match lista with
    | [] -> Printf.fprintf oc "%s" "";                      (* Não escreve no final do ficheiro *)
    | h::t -> Printf.fprintf oc "%s\n" h; escrever t        (* Escreve resultado*)
  )in escrever resultado;
  close_out oc;                                             (* Fecha o ficheiro*)
);;


(************************
 * Execução do Programa *
 ***********************)

let dadosLista = lerFicheiro "dados.txt";;    (* Ler os dados do ficheiro *)
let res = resultados (analisar dadosLista);;  (* Analisar os dados e imprimir*)
imprimirResultado (res);;                     (* Imprimir no Ecrã os resultados*) 
gravarResultado "resultados.txt" res;;        (* Gravar resultados num ficheiro *)
