<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar armazéns e moinhos (warehouse e mills). Ele permite a exibição, edição e validação de dados relacionados a esses elementos em uma grade (grid). O objetivo principal é facilitar a manipulação de dados de armazéns e moinhos, incluindo a adição, exclusão e validação de registros.

* **Tecnologias Utilizadas:**
  - Delphi (VCL e componentes visuais como `TcxGrid`, `TcxEditRepository`, etc.).
  - SOAP para comunicação com serviços externos.
  - Manipulação de datasets com `TClientDataSet`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas da Grade e seus Tipos:**
      - `mill` (string): Representa o moinho.
      - `millWhse` (string): Representa o armazém do moinho.
      - `sapCode` (string): Código SAP.
      - `stat` (string): Status.
    - **Ações da Grade e seus Efeitos:**
      - Adicionar (`ADD`): Permite adicionar novos registros.
      - Excluir (`DELETE`): Permite excluir registros existentes.
      - Edição direta: Permite editar valores diretamente na grade.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar novos registros.
  - Excluir registros existentes.
  - Editar valores diretamente na grade.
  - Validar dados antes de salvar.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato de tabela.
  - `TcxEditRepository`: Define editores personalizados para os campos.
  - `TClientDataSet`: Gerencia os dados exibidos na grade.
  - `TMillServiceUtils`: Utilitário para interagir com serviços relacionados a moinhos.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor do campo na grade for alterado então
        validar o campo
    ```
  - Ação `ACTaddExecute`:
    ```pseudo
    se botão "Adicionar" for clicado então
        criar novo registro
    ```
  - Evento `BeforePost`:
    ```pseudo
    se registro for salvo então
        validar os dados
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização:
     - O componente é criado e configurado no construtor `Create`.
     - Configurações da grade, como campos ocultos, ordem e editores personalizados, são definidas.
  2. Interação do Usuário:
     - O usuário pode adicionar, excluir ou editar registros diretamente na grade.
     - Eventos como `OnEditValueChanged` e `BeforePost` são disparados para validação e manipulação de dados.
  3. Funções Executadas:
     - `m_FindMill` (arquivo: `MillServiceUtils`): Realiza busca de moinhos.
     - `m_Validate` (arquivo: `kneFRGridEditSOA`): Valida os dados antes de salvar.

* **Dados Necessários:**
  - Código do armazém (`warehouseCode`).
  - Código do moinho (`mill`).
  - Código SAP (`sapCode`).
  - Status (`stat`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um registro estiver selecionado.
  - **Salvar:** Apenas se todos os campos obrigatórios forem preenchidos e válidos.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Registro duplicado" se um registro com os mesmos valores já existir.

* **Valores Padrão dos Campos:**
  - Não há valores padrão definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - `mill`: Deve ser preenchido.
  - `millWhse`: Deve ser preenchido.
  - `stat`: Deve ser preenchido.

---

## 5. Funções Principais:

* **`m_FindMill`:** Realiza a busca de moinhos utilizando um diálogo de busca.
* **`m_Validate`:** Valida os dados antes de salvar.
* **`SetMillsWhseDesc`:** Atualiza a descrição do armazém do moinho.
* **`m_ExistsRecord`:** Verifica se um registro já existe no dataset.

---

## 6. Consumo de Serviços API:

* **Serviço:** `MillServiceUtils`.
  - **Endpoint:** Não especificado no código.
  - **Dados Enviados:** Não especificado no código.
  - **Dados Recebidos:** Não especificado no código.
  - **Propósito:** Buscar informações sobre moinhos.
  - **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos explicitamente no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `cxGrid`, `cxEditRepository`: Para exibição e edição de dados.

* **Componentes Customizados:**
  - `TMillServiceUtils`: Utilitário para interagir com serviços relacionados a moinhos.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `mill` (string, obrigatório).
  - `millWhse` (string, obrigatório).
  - `sapCode` (string, opcional).
  - `stat` (string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `mill` → `mill`.
  - `millWhse` → `millWhse`.
  - `sapCode` → `sapCode`.
  - `stat` → `stat`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEwarehouseMills := TFRAMEwarehouseMills.Create(Self);
  FRAMEwarehouseMills.SetMillsWhseDesc('OldWhse', 'NewWhse');
  ```
* **HTML Renderizado:**
  ```html
  <table style="border: 1px solid black; border-collapse: collapse;">
    <tr>
      <th style="border: 1px solid black;">Mill</th>
      <th style="border: 1px solid black;">MillWhse</th>
      <th style="border: 1px solid black;">SAP Code</th>
      <th style="border: 1px solid black;">Status</th>
    </tr>
    <tr>
      <td style="border: 1px solid black;">Moinho 1</td>
      <td style="border: 1px solid black;">Armazém 1</td>
      <td style="border: 1px solid black;">SAP001</td>
      <td style="border: 1px solid black;">Ativo</td>
    </tr>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades no construtor `Create`.
* Definição de campos ocultos e ordem de exibição na grade.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar dados de armazéns e moinhos, com suporte a validação e integração com serviços externos. No entanto, faltam detalhes sobre endpoints e tratamento de erros em chamadas de serviços.

---

## 13. Resumo Curto:

O componente `TFRAMEwarehouseMills` gerencia dados de armazéns e moinhos em uma grade, permitindo adição, exclusão e validação de registros, com suporte a integração SOAP.#### **FRwarehouseMills.pas**

```
unit FRwarehouseMills;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEwarehouseMills = class(TFRAMEBaseGridEditSOA)
    cxEDTmillWhse: TcxEditRepositoryMaskItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_FindByCodeMill(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindMill(Sender: TObject; AButtonIndex: Integer);
    function m_ExistsRecord(const pv_dataset: TClientDataSet;
      const pv_fieldName: TStringList): Boolean;
  protected
    { Protected declarations }
    function m_Validate: Boolean;  override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetMillsWhseDesc(pv_oldMillWhse, pv_Millwhse: string);
  end;

var
  FRAMEwarehouseMills: TFRAMEwarehouseMills;

implementation

uses
  kneFindDialogSOA, kneUtils, kneTypes, kneFindDialog, kneDialogFactory, 
  kneFGFindUtils, Global,
  //---
  MillServiceUtils, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEwarehouseMills.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode=whse';
  DataPacketName := 'MillWarehouse';              // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'mills';                        // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; millWhse; sapCode; stat');  //millDtime; gpsDtime;
    // Key Fields ..............................................................
    KeyFields:= 'whse;millWhse';
    // Custom Editors ..........................................................
    AddCustomField('mill','cxEDTfind');
    AddCustomField('millWhse','cxEDTmillWhse');
    AddCustomField('stat','cxEDTstat');
  end; //with
  UseColsBestFit := False;
  ColsWidthInGrid := '60;100;90;90';
  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindMill;
end;

procedure TFRAMEwarehouseMills.m_FindMill(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Service: TMillServiceUtils;
  lv_FindOptions: TFindDialogSettings;
  lv_FindResult: integer;
  lv_Find: TFORMkneFindDialog;
begin
  lv_Find := nil;
  try
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find do
    begin

```

#### **FRwarehouseMills.dfm**

```
inherited FRAMEwarehouseMills: TFRAMEwarehouseMills
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTmillWhse: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
    end
  end
end
```
<!-- tabs:end -->

