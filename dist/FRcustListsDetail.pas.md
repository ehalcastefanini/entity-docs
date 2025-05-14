<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código `FRcustListsDetail` implementa uma interface para gerenciar uma lista de clientes em um sistema. Ele permite a visualização, edição e importação de dados relacionados a clientes. O objetivo principal é fornecer uma interface de usuário para manipular dados de clientes de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - Delphi (VCL e componentes visuais como `TcxGrid`, `TsPanel`, `TsBitBtn`).
  - SOAP para comunicação com serviços externos.
  - Manipulação de banco de dados com `DBClient`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `custCd` (Código do Cliente, string).
      - `abbrName` (Nome Abreviado, string).
    - **Ações do Grid e seus Efeitos:**
      - Edição de valores diretamente no grid.
      - Importação de dados via botão "Import".
      - Adição e exclusão de registros.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Importar dados de clientes de uma fonte externa.
  - Adicionar novos clientes à lista.
  - Editar informações de clientes diretamente no grid.
  - Validar campos obrigatórios antes de salvar.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato de tabela.
  - `TsPanel` e `TsBitBtn`: Painéis e botões para ações específicas.
  - Métodos privados e públicos para manipulação de dados e eventos.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Import":
    ```pseudo
    if botão "Import" clicado então executar m_PasteDataFromExcel.
    ```
  - Evento `OnEditValueChanged` no grid:
    ```pseudo
    if valor de célula alterado então validar e processar a alteração.
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com o método `Create`.
  2. Configuração do grid e propriedades relacionadas.
  3. Interação do usuário com o grid ou botões dispara eventos.
  4. Eventos chamam métodos específicos para processar ações.

* **Dados Necessários:**
  - Código do cliente (`custCd`).
  - Nome abreviado do cliente (`abbrName`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Import" só deve ser clicado se houver dados válidos para importar.
  - Edição no grid só é permitida em colunas específicas.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Cliente já adicionado" se o cliente já existir na lista.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validação de Campos:**
  - `custCd`: Deve ser único e não vazio.
  - `abbrName`: Deve ser preenchido.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_FindCustomer`: Abre um diálogo para buscar clientes.
  - `m_ValidateEmptyFields`: Valida se todos os campos obrigatórios estão preenchidos.
  - `m_PasteDataFromExcel`: Processa dados copiados de uma planilha Excel.
  - `SetColumnState`: Configura o estado de edição de uma coluna específica.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CustomerListServiceUtils`.
  - Endpoint: Não especificado no código.
  - Dados Enviados: Não especificado no código.
  - Dados Recebidos: Não especificado no código.
  - Propósito: Gerenciar dados de clientes.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `cxGrid`, `cxDBData`: Para exibição de dados em formato de grid.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o frame.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `custCd` (tipo: string, obrigatório).
  - `abbrName` (tipo: string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `custCd` mapeado para a coluna `custCd`.
  - `abbrName` mapeado para a coluna `abbrName`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  procedure TFRAMEcustListsDetail.BTNimportClick(Sender: TObject);
  begin
    m_PasteDataFromExcel;
  end;
  ```
* **HTML Representando o Grid:**
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th>custCd</th>
        <th>abbrName</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>C001</td>
        <td>Cliente A</td>
      </tr>
      <tr>
        <td>C002</td>
        <td>Cliente B</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* `mv_AlreadyAdded`: Variável usada para verificar se um cliente já foi adicionado.
* `mc_Fields`: Define os campos principais do grid.

---

## 12. Conclusão:

O código `FRcustListsDetail` é uma implementação robusta para gerenciar listas de clientes. Ele oferece funcionalidades como edição, importação e validação de dados. No entanto, faltam detalhes sobre endpoints de serviços e validações mais avançadas.

---

## 13. Resumo Curto:

O `FRcustListsDetail` é um frame para gerenciar listas de clientes, permitindo edição, importação e validação de dados em um grid interativo, com suporte a serviços SOAP para integração.#### **FRcustListsDetail.pas**

```
unit FRcustListsDetail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEcustListsDetail = class(TFRAMEBaseGridEditSOA)
    PNLimport: TsPanel;
    BTNimport: TsBitBtn;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure BTNimportClick(Sender: TObject);
  private
    mv_AlreadyAdded : string; // [29-06-2018, #23438]
    procedure m_FindCustomer(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindCustomerByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_AlreadyAdded;
    procedure m_ProcessEachRow(pv_RowIndex: Integer; pv_RowInfo: TcxRowInfo);
    procedure m_ValidateEmptyFields;
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure m_PasteDataFromExcel;
    procedure SetKeyEditing(const EditKey: Boolean); override;
  end;

var
  FRAMEcustListsDetail: TFRAMEcustListsDetail;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, Global, kneUtils, Clipbrd
  , kneFGGenericUtils
  , FRfindCriteriaCustomer
  , BaseServiceUtils
  , CustomerSimpleServiceUtils
  , SimpleCustomerToAddServiceUtils
  , CustomerWithGenericCriteriaServiceUtils
  , CustomerListServiceUtils
  , kneFREditSOA;

const
  mc_Fields: string = 'custCd;abbrName';

{$R *.dfm}

constructor TFRAMEcustListsDetail.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'custListCd';
  DataPacketName := 'CustomerListDet';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'customerListDet';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    HiddenFields.Add('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_Fields);

    // Key Fields ..............................................................
    KeyFields:= 'custCd;custListCd';
    // Custom Editors ..........................................................
    AddCustomField('custCd','cxEDTfind');

  end; //with

  cxEDTfind.Properties.OnButtonClick := m_FindCustomer;

  mv_AlreadyAdded := ''; // [29-06-2018, #23438]
end;

procedure TFRAMEcustListsDetail.m_FindCustomer(
  Sender: TObject; AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
  lv_FieldName : string;
```

#### **FRcustListsDetail.dfm**

```
inherited FRAMEcustListsDetail: TFRAMEcustListsDetail
  Width = 802
  ParentFont = True
  inherited cxDBG: TcxGrid
    Width = 802
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      DataController.DataModeController.GridMode = False
    end
  end
  inherited PNLfooter: TsPanel
    Width = 802
    inherited PNLeditActions: TsPanel
      Width = 407
      object PNLimport: TsPanel
        Left = 321
        Top = 1
        Width = 80
        Height = 30
        Align = alLeft
        TabOrder = 4
        SkinData.SkinSection = 'ALPHACOMBOBOX'
        object BTNimport: TsBitBtn
          Left = 3
          Top = 3
          Width = 70
          Height = 24
          Caption = 'Import'
          TabOrder = 0
          OnClick = BTNimportClick
          NumGlyphs = 2
          SkinData.SkinSection = 'BUTTON'
          ImageIndex = 6
          Images = IMLeditActions
        end
      end
    end
  end
  inherited IMLeditActions: TImageList
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
```
<!-- tabs:end -->

