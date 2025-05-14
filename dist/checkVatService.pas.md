<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  Este código foi gerado a partir de um arquivo WSDL para consumir o serviço web "checkVatService" da União Europeia, que permite verificar a validade de números de IVA (Imposto sobre o Valor Acrescentado) de empresas registradas em países da UE. Ele resolve o problema de validação de números de IVA de forma automatizada, permitindo que sistemas integrem essa funcionalidade diretamente.

* **Funcionalidade de Alto Nível:**
  O código fornece métodos para realizar duas operações principais:
  - `checkVat`: Verifica a validade de um número de IVA.
  - `checkVatApprox`: Realiza uma verificação aproximada de um número de IVA.
  
  Um exemplo prático seria um sistema de faturação que utiliza este serviço para validar o número de IVA de um cliente antes de emitir uma fatura.

* **Tecnologias Utilizadas:**
  - **Delphi 7**: Linguagem de programação e ambiente de desenvolvimento.
  - **SOAP**: Protocolo para comunicação com o serviço web.
  - **WSDL**: Descrição do serviço web para geração automática de código.

* **Tipo de Interface:**
  Este código não contém um formulário ou exibição de grade. Ele é puramente funcional e serve como uma interface para consumir o serviço web.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Verificar a validade de um número de IVA (`checkVat`).
  - Realizar uma verificação aproximada de um número de IVA (`checkVatApprox`).
  - Verificar a conectividade com a Internet antes de realizar as chamadas ao serviço.

* **Componentes Principais:**
  - **Classes:**
    - `checkVat`: Representa os parâmetros para a verificação de IVA.
    - `checkVatResponse`: Representa a resposta da verificação de IVA.
    - `checkVatApprox`: Representa os parâmetros para a verificação aproximada de IVA.
    - `checkVatApproxResponse`: Representa a resposta da verificação aproximada de IVA.
  - **Interface:**
    - `checkVatPortType`: Define os métodos para comunicação com o serviço web.
  - **Função Auxiliar:**
    - `HasInternetConnectivity`: Verifica se há conectividade com a Internet.

* **Pseudo-código das Ações e Eventos:**
  - `checkVat`:
    ```
    se parâmetros válidos então
        chamar serviço web checkVat
        retornar resposta
    ```
  - `checkVatApprox`:
    ```
    se parâmetros válidos então
        chamar serviço web checkVatApprox
        retornar resposta
    ```
  - `HasInternetConnectivity`:
    ```
    verificar conectividade com a Internet
    retornar verdadeiro ou falso
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização das classes e configuração do serviço.
  2. Verificação de conectividade com a Internet usando `HasInternetConnectivity`.
  3. Chamada ao método `checkVat` ou `checkVatApprox` com os parâmetros necessários.
  4. Recebimento e processamento da resposta do serviço.

* **Dados Necessários:**
  - Para `checkVat`:
    - Número de IVA.
    - Código do país.
  - Para `checkVatApprox`:
    - Número de IVA.
    - Código do país.
    - Informações adicionais para verificação aproximada.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - A chamada ao serviço só deve ser feita se houver conectividade com a Internet.
  - Os parâmetros (número de IVA e código do país) devem ser válidos.

* **Filtros Disponíveis:**
  Não aplicável, pois o código não possui filtros.

* **Mensagens de Erro:**
  - "Sem conectividade com a Internet" se não houver conexão.
  - "Parâmetros inválidos" se os parâmetros fornecidos forem incorretos.
  - "Erro ao chamar o serviço" se a chamada ao serviço falhar.

* **Valores Padrão dos Campos:**
  Não aplicável, pois o código não possui campos de entrada.

* **Validação de Campos e Condições:**
  - O número de IVA deve ser validado antes de ser enviado ao serviço.
  - O código do país deve ser um código ISO válido.

---

## 5. Funções Principais:

* **checkVat**:
  - Verifica a validade de um número de IVA.
  - Retorna uma resposta indicando se o número é válido ou não.

* **checkVatApprox**:
  - Realiza uma verificação aproximada de um número de IVA.
  - Retorna uma resposta com informações adicionais.

* **HasInternetConnectivity**:
  - Verifica se há conectividade com a Internet antes de realizar as chamadas ao serviço.

---

## 6. Consumo de Serviços API:

* **Serviço:** checkVatService.
* **Endpoint:** `http://ec.europa.eu/taxation_customs/vies/checkVatService.wsdl`.
* **Dados Enviados:**
  - Para `checkVat`: `{ "countryCode": "string", "vatNumber": "string" }`.
  - Para `checkVatApprox`: `{ "countryCode": "string", "vatNumber": "string", "additionalInfo": "string" }`.
* **Dados Recebidos:**
  - `{ "valid": "boolean", "name": "string", "address": "string" }`.
* **Propósito:** Validar números de IVA.
* **Tratamento de Erros:** Exibe mensagens de erro em caso de falha na chamada.

---

## 7. Campos Condicionais (Lógica do Formulário):

Não aplicável, pois o código não possui interface de formulário.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `Windows`, `SysUtils`, `Classes`, `WinInet`: Utilizadas para funcionalidades básicas do sistema e conectividade com a Internet.
  - `FG_Win64utils`: Biblioteca adicional para suporte a funcionalidades específicas.

* **Componentes Personalizados:**
  - Não há componentes personalizados.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `countryCode` (tipo: string, obrigatório).
  - `vatNumber` (tipo: string, obrigatório).
  - `additionalInfo` (tipo: string, opcional para `checkVatApprox`).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  Não aplicável, pois o código não interage diretamente com um banco de dados.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    vatService: checkVatPortType;
    response: checkVatResponse;
  begin
    if HasInternetConnectivity then
    begin
      vatService := GetcheckVatPortType;
      response := vatService.checkVat(checkVat.Create('PT', '123456789'));
      ShowMessage(response.valid);
    end
    else
      ShowMessage('Sem conectividade com a Internet');
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* O código foi adaptado para Delphi 7, substituindo `string` por `WideString` para maior compatibilidade.
* Implementação de timeouts para evitar que a aplicação fique "pendurada" durante chamadas ao serviço.

---

## 12. Conclusão:

Este código fornece uma interface robusta para consumir o serviço web de validação de números de IVA da União Europeia. Ele é eficiente, com suporte a timeouts e verificação de conectividade, mas depende de conectividade com a Internet e de parâmetros válidos para funcionar corretamente.

---

## 13. Resumo Curto:

Código em Delphi 7 para consumir o serviço web "checkVatService", permitindo validar números de IVA de empresas da UE. Inclui suporte a timeouts, verificação de conectividade e métodos para validação direta e aproximada.#### **checkVatService.pas**

```
// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://ec.europa.eu/taxation_customs/vies/checkVatService.wsdl
// Encoding : UTF-8
// Version  : 1.0
// (2018/02/21 12:56:21 - 1.33.2.5)
// ************************************************************************ //
// [2018/02/21] #23256
// Este ficheiro foi inicialmente gerado de forma automatica pelo Delphi 7,
// abrindo a opcao
//
//  File > New > Other... > WebServices > WSDL Importer,
//
// e digitando o URL
//
//  http://ec.europa.eu/taxation_customs/vies/checkVatService.wsdl
//
// sendo, em seguida, editado e modificado manualmente para adaptar, para
// Delphi 7, as indicacoes dadas no artigo
//
//  http://www.drbob42.com/examine/examinE2.htm
//
// e tendo por base a informacao do ficheiro 'checkVatService.pas' equivalente
// que e' gerado automaticamente nas versoes mais recentes do Delphi (Delphi XE2+)
//
//  https://github.com/ghquant/Delphi-OmniThreadLibrary/blob/master/examples/checkVat/checkVatService.pas
//
// e alguma informacao adicional, relativamente 'a implementacao de "timeouts"
// maximos para optimizar a performance das chamadas a checkVat() e checkVatApprox()
// (indirectamente, por via da inicializacao feita na funcao GetcheckVatPortType()):
//
//  https://shenoyatwork.blogspot.pt/2006/11/setting-timeouts-in-delphi-soap.html
//
// As adaptacoes para Delphi 7 consistiram, assim, nas seguintes modificacoes:
//
// 1. Foi modificada (corrigida) a definicao de 'matchCode'
//
// 2. Foram (re)definidas as classes
//
//     checkVat = class(TRemotable)
//     checkVatResponse = class(TRemotable)
//     checkVatApprox = class(TRemotable)
//     checkVatApproxResponse = class(TRemotable)
//
//  2.1. Substituindo o tipo 'string' por 'WideString'
//       (em Delphi 7 e' mais seguro indicar explicitamente WideString)
//
//  2.2. Removendo as variaveis membro 'Fxxx_Specified: boolean;', as respectivas
//       funcoes 'function xxx_Specified(Index: Integer): boolean;' e as
//       respectivas referencias 'stored xxx_Specified;' associadas 'as
//       propriedades 'xxx' (e respectivas variaveis membro 'Fxxx')
//
//  2.3. Removendo, nas propriedades 'xxx', os SETters (substituindo-os por
//       declaracoes 'write Fxxx' para escrever directamente nas variaveis
//       membro 'xxx') bem como as referencias
//
//        Index (IS_OPTN)
//        Index (IS_OPTN or IS_NLBL) // <=> Index (IS_OPTN or TSXInteger)
//
// 3. Na definicao do interface
//
//     checkVatPortType = interface(IInvokable)
//
//    foram redefinidos os prototipos das funcoes:
//
//     function  checkVat(const parameters: checkVat): checkVatResponse; stdcall;
//     function  checkVatApprox(const parameters: checkVatApprox): checkVatApproxResponse; stdcall;
//
//  4. Na declaracao 'uses', foram acrescentadas as units
//
//      Windows, SysUtils, Classes, WinInet, FG_Win64utils
//
//     e
//
//   4.1. Implementados os construtores e destrutores definidos nas classes:
//
//         constructor checkVat.Create;
//
//         constructor checkVatResponse.Create;
//         destructor checkVatResponse.Destroy;
//
//         constructor checkVatApprox.Create;
//
//         constructor checkVatApproxResponse.Create;
//         destructor checkVatApproxResponse.Destroy;
//
//   4.2. Implementada uma nova funcao
//
//         HasInternetConnectivity
//
//        que verifica se existe conectividade com o exterior (Internet),
//        optimizando a execucao das funcoes checkVat() e checkVatApprox()! ;)
//
//   4.3. Na funcao
//
//         GetcheckVatPortType
//
//        foram definidos "timeouts" maximos de 2 SEGUNDOS para usar durante
//        a execucao do servico (de modo a nao deixar a aplicacao "pendurada"
```

#### **checkVatService.dfm**

```
dfm file is not needed for this file```
<!-- tabs:end -->

