<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado define um componente de interface gráfica chamado `TFRAMEebEntityLinkOldValue`, que herda de `TFRAMEBaseCtrlEditSOA`. Este componente é utilizado para exibir e gerenciar dados relacionados a uma entidade chamada `EbEntityLink`. O objetivo principal é evitar atualizações automáticas de dados mestre/detalhe, garantindo que os dados exibidos na interface permaneçam inalterados.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar o componente.
  - **VCL (Visual Component Library):** Biblioteca de componentes visuais do Delphi.
  - **SOAP:** Protocolo utilizado para comunicação com serviços web.
  - **DBClient:** Componente para manipulação de dados em memória.

* **Forma do Componente:**
  - Este componente não é um formulário nem uma grade de exibição. Ele é uma estrutura de interface (`Frame`) que gerencia dados relacionados a uma entidade.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - O componente é responsável por exibir dados relacionados à entidade `EbEntityLink` sem permitir alterações nos mesmos.
  - Ele desativa a atualização automática entre os dados mestre e detalhe.

* **Componentes Principais:**
  - `MasterKeyFields`: Define os campos de chave mestre. Neste caso, está vazio para evitar atualizações automáticas.
  - `DataPacketName`: Nome do pacote de dados relacionado à entidade.
  - `PropertyName`: Nome do campo que contém os detalhes da entidade.
  - `FrameType`: Define o tipo de frame como `frtDetail`.
  - `ShowActionPanel`: Desativa o painel de ações.
  - `AvailableActions`: Define as ações disponíveis (nenhuma neste caso).

* **Pseudo-código das Ações e Eventos:**
  - Inicialização do Frame:
    ```pseudo
    ao criar o frame:
        desativar atualizações automáticas mestre/detalhe
        definir o nome do pacote de dados como 'EbEntityLink'
        definir o nome da propriedade como 'old'
        configurar o tipo de frame como detalhe
        ocultar o painel de ações
        desativar ações disponíveis
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. O componente é inicializado através do construtor `Create`.
  2. Durante a inicialização:
     - As propriedades do frame são configuradas para evitar atualizações automáticas.
     - O nome do pacote de dados e a propriedade são definidos.
     - O painel de ações é desativado.

* **Dados Necessários:**
  - Nenhum dado precisa ser preenchido pelo usuário, pois o frame é configurado para exibir dados estáticos.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Nenhuma ação interativa está disponível para o usuário, pois o painel de ações está desativado.

* **Filtros Disponíveis:**
  - Não aplicável, pois o frame não possui filtros configurados.

* **Mensagens de Erro:**
  - Não há mensagens de erro definidas no código.

* **Valores Padrão dos Campos:**
  - `MasterKeyFields`: Valor padrão é vazio (`''`).
  - `DataPacketName`: Valor padrão é `'EbEntityLink'`.
  - `PropertyName`: Valor padrão é `'old'`.
  - `FrameType`: Valor padrão é `frtDetail`.
  - `ShowActionPanel`: Valor padrão é `False`.
  - `AvailableActions`: Valor padrão é vazio (`''`).

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`Create(AOwner: TComponent)`**
  - Configura as propriedades do frame para evitar atualizações automáticas e define os valores padrão para exibição de dados relacionados à entidade `EbEntityLink`.

---

## 6. Consumo de Serviços API:

* Não há chamadas a serviços externos definidas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `InvokeRegistry`, `SOAPHTTPClient`: Utilizadas para comunicação com serviços SOAP.
  - `DB`, `DBClient`: Utilizadas para manipulação de dados em memória.

* **Componentes Customizados:**
  - `TFRAMEBaseCtrlEditSOA`: Componente base do qual o frame herda funcionalidades.

---

## 9. Listagem de Campos e Validações:

* **Campos Definidos no Código:**
  - `MasterKeyFields` (tipo: string, valor padrão: vazio).
  - `DataPacketName` (tipo: string, valor padrão: `'EbEntityLink'`).
  - `PropertyName` (tipo: string, valor padrão: `'old'`).
  - `FrameType` (tipo: enumeração, valor padrão: `frtDetail`).
  - `ShowActionPanel` (tipo: booleano, valor padrão: `False`).
  - `AvailableActions` (tipo: string, valor padrão: vazio).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  ```plaintext
  Início -> Inicialização do Frame -> Configuração de Propriedades -> Fim
  ```

* **Diagrama de Sequência:**  
  ```plaintext
  Usuário -> Inicializa Frame -> Configurações Aplicadas -> Dados Exibidos
  ```

* **Trecho de Código:**
  ```delphi
  constructor TFRAMEebEntityLinkOldValue.Create(AOwner: TComponent);
  begin
    inherited;
    MasterKeyFields := '';
    DataPacketName := 'EbEntityLink';
    PropertyName := 'old';
    FrameType := frtDetail;
    ShowActionPanel := False;
    AvailableActions := '';
  end;
  ```

* **HTML Representando o Template:**  
  Não aplicável, pois o arquivo `.dfm` não foi fornecido.

---

## 11. Comentários Importantes no Código:

* **Comentários no Construtor:**
  - Explicação sobre a desativação de atualizações automáticas mestre/detalhe.
  - Justificativa para não definir campos de chave mestre.

---

## 12. Conclusão:

O código define um frame especializado para exibir dados relacionados à entidade `EbEntityLink` sem permitir alterações. Ele é útil em cenários onde os dados devem ser exibidos de forma estática. No entanto, o código não possui validações ou interações configuradas, limitando sua funcionalidade.

---

## 13. Resumo Curto:

O componente `TFRAMEebEntityLinkOldValue` exibe dados estáticos relacionados à entidade `EbEntityLink`, desativando atualizações automáticas mestre/detalhe. Ele é configurado para exibição sem interatividade, sendo ideal para cenários de consulta de dados.#### **FRebEntityLinkOldValue.pas**

```
unit FRebEntityLinkOldValue;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel;

type
  TFRAMEebEntityLinkOldValue = class(TFRAMEBaseCtrlEditSOA)
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent);  override;
    { Public declarations }
  end;

var
  FRAMEebEntityLinkOldValue: TFRAMEebEntityLinkOldValue;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes;

constructor TFRAMEebEntityLinkOldValue.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  // masterfields fica com nenhum campo para evitar a actualiza��o automatica master/detail
  // principalmente pq quase todos os campos da entidade s�o keys.
  // ou seja, por comportamento esperado, o master actualiza seus clients que se relacionam pelas suas keys.
  // e neste caso, como n�o queremos que os dados nesta frame seja alterados, simplesmente n�o colocamos os campos q relacionam.
  MasterKeyFields := '';
  DataPacketName := 'EbEntityLink';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'old';         // nome do campo da metadata(entidadeMaster) que vai conter os details
  FrameType := frtDetail;

  ShowActionPanel := False;
  AvailableActions := '';
end;

end.
```

#### **FRebEntityLinkOldValue.dfm**

```
inherited FRAMEebEntityLinkOldValue: TFRAMEebEntityLinkOldValue
end
```
<!-- tabs:end -->

