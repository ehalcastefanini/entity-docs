<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código `FRagentsCustomers` implementa uma interface para gerenciar a relação entre agentes e clientes em um sistema. Ele permite visualizar, adicionar e editar informações relacionadas a agentes e seus clientes, incluindo taxas de comissão e tipos de comissão. O objetivo é facilitar a manipulação de dados de forma estruturada e eficiente.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação principal.
  - **Componentes cxGrid:** Para exibição e manipulação de dados em formato de tabela.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **DBClient:** Para manipulação de dados em memória.
  - **kneUtils e kneFGEnv:** Bibliotecas utilitárias personalizadas.

* **Forma do Componente:**  
  - **Exibição em Grade (Grid Display):**  
    - **Colunas da Grade e Tipos:**  
      - `stat` (string): Status do cliente.
      - `mill` (string): Nome do moinho.
      - `customer` (string): Código do cliente.
      - `customerName` (string): Nome do cliente.
      - `commRate` (decimal): Taxa de comissão.
      - `commType` (string): Tipo de comissão.
      - `lastUpd` (datetime): Última atualização.
      - `updBy` (string): Usuário que realizou a última atualização.
    - **Ações da Grade e Efeitos:**  
      - Adicionar (`ADD`): Permite adicionar novos registros de clientes.
      - Excluir (`DELETE`): Remove registros existentes.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**  
  - Adicionar novos clientes a um agente.
  - Editar informações de clientes existentes.
  - Excluir clientes associados a um agente.
  - Filtrar e buscar clientes por código ou nome.

* **Componentes Principais:**  
  - `cxGrid`: Exibe os dados em formato de tabela.
  - `cxEDTfindCustomer`: Botão para buscar clientes.
  - `cxEDTcommRate`: Campo para editar a taxa de comissão.
  - `cxICBcommType`: Campo para selecionar o tipo de comissão.

* **Tradução para Pseudo-código:**  
  - Evento `OnEditValueChanged`:  
    ```pseudo
    se valor do campo editado mudar então
        validar e atualizar o valor
    ```
  - Evento `OnClick` do botão "Adicionar":  
    ```pseudo
    se botão "Adicionar" for clicado então
        abrir formulário para adicionar novo cliente
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. Inicialização do componente: Configurações da grade e propriedades são definidas.
  2. Carregamento de dados: Dados de agentes e clientes são carregados do serviço SOAP.
  3. Interações do usuário:  
     - O usuário pode adicionar, editar ou excluir registros.
     - Ações específicas, como buscar clientes, são realizadas por meio de botões e eventos.

* **Dados Necessários:**  
  - Código do agente.
  - Informações do cliente (nome, código, taxa de comissão, tipo de comissão).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - **Adicionar Cliente:** Requer que o código do agente esteja definido.
  - **Excluir Cliente:** Requer que um cliente esteja selecionado na grade.

* **Filtros Disponíveis:**  
  - Filtro por código do cliente.
  - Filtro por nome do cliente.

* **Mensagens de Erro:**  
  - "Cliente já adicionado" se o cliente já estiver associado ao agente.
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.

* **Valores Padrão dos Campos:**  
  - `commRate`: Padrão "0.00".
  - `commType`: Padrão "Percentual".

* **Validações e Condições dos Campos:**  
  - `commRate`: Deve ser um número decimal entre 0 e 100.
  - `commType`: Deve ser selecionado entre as opções disponíveis.

---

## 5. Funções Principais:

* **`ShowData`:** Carrega e exibe os dados na grade.
* **`m_FindCustomer`:** Busca clientes por código ou nome.
* **`m_AdjustCtrlsState`:** Ajusta o estado dos controles com base no tipo de agente.
* **`m_CheckTotalValue`:** Verifica o valor total dos clientes associados.

---

## 6. Consumo de Serviços API:

* **Serviço:** `CustomersInMarketsServiceUtils`  
  - **Endpoint:** `/api/customers`  
  - **Dados Enviados:** `{ "agentCode": "string", "customerCode": "string" }`  
  - **Dados Recebidos:** `{ "status": "success", "data": "Customer object" }`  
  - **Propósito:** Obter informações de clientes associados a agentes.  
  - **Tratamento de Erros:** Exibe mensagem de erro se a chamada falhar.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo `commRate` só é habilitado se o tipo de comissão (`commType`) for "Percentual".

---

## 8. Dependências:

* **Bibliotecas Externas:**  
  - `cxGrid`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Personalizados:**  
  - `kneUtils`: Funções utilitárias.
  - `kneFGEnv`: Configurações globais do sistema.

---

## 9. Listagem de Campos e Validações:

* **Campos:**  
  - `stat` (string, opcional): Status do cliente.
  - `mill` (string, opcional): Nome do moinho.
  - `customer` (string, obrigatório): Código do cliente.
  - `customerName` (string, obrigatório): Nome do cliente.
  - `commRate` (decimal, obrigatório): Taxa de comissão (0-100).
  - `commType` (string, obrigatório): Tipo de comissão.
  - `lastUpd` (datetime, automático): Última atualização.
  - `updBy` (string, automático): Usuário que realizou a última atualização.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  FRAMEagentsCustomers := TFRAMEagentsCustomers.Create(Self);
  FRAMEagentsCustomers.ShowData;
  ```

* **HTML Renderizado:**  
  ```html
  <table style="border: 1px solid black; width: 100%;">
    <thead>
      <tr>
        <th>Status</th>
        <th>Moinho</th>
        <th>Código do Cliente</th>
        <th>Nome do Cliente</th>
        <th>Taxa de Comissão</th>
        <th>Tipo de Comissão</th>
        <th>Última Atualização</th>
        <th>Atualizado Por</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Ativo</td>
        <td>Moinho A</td>
        <td>12345</td>
        <td>Cliente A</td>
        <td>10.00</td>
        <td>Percentual</td>
        <td>2023-10-01</td>
        <td>Admin</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração da Grade:**  
  ```delphi
  DefineOrderFields('stat; mill; customer; customerName; commRate; commType; lastUpd; updBy;');
  ```

* **Validação de Taxa de Comissão:**  
  ```delphi
  Properties.EditMask := '\d{1,2}(\.\d{1,2})?';
  ```

---

## 12. Conclusão:

O código `FRagentsCustomers` é uma implementação robusta para gerenciar a relação entre agentes e clientes. Ele oferece funcionalidades completas para manipulação de dados, mas depende de serviços externos para buscar informações. Uma limitação é a falta de validações mais detalhadas diretamente no código.

---

## 13. Resumo Curto:

O `FRagentsCustomers` gerencia agentes e clientes, permitindo adicionar, editar e excluir registros. Ele utiliza uma grade para exibição de dados e integra-se a serviços SOAP para buscar informações. É uma solução eficiente para sistemas de gerenciamento de clientes.#### **FRagentsCustomers.pas**

```
unit FRagentsCustomers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid
  , cxImageComboBox;

type
  TFRAMEagentsCustomers = class(TFRAMEBaseGridEditSOA)
    cxEDTfindCustomer: TcxEditRepositoryButtonItem;
    cxEDTcommRate: TcxEditRepositoryMaskItem;
    cxICBcommType: TcxEditRepositoryImageComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    FAlreadyAdded : string;
    FCustsTotalValue: Currency;

    procedure ShowData; override;
    procedure m_FindByCodeMill(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindCustomerByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindMill(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindCustomer(Sender: TObject; AButtonIndex: Integer);
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    function m_AlreadyAdded: string;
    procedure m_ProcessEachRow(pv_RowIndex: Integer;
      pv_RowInfo: TcxRowInfo);
    function GetCustsTotalValue: Currency;
    procedure m_CheckTotalValue;
    procedure m_SumColValue(pv_RowIndex: Integer; pv_RowInfo: TcxRowInfo);
    procedure m_SetCommRateMask;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure m_AdjustCtrlsState(const pv_AgentTp: string);

  published
    property CustsTotalValue: Currency read GetCustsTotalValue;


  end;

var
  FRAMEagentsCustomers: TFRAMEagentsCustomers;

implementation

uses
  kneUtils, kneFGEnv, kneTypes, Global, kneFindDialogSOA, kneFindDialog, kneDialogFactory,
  kneFGDBUtils, kneFGFindUtils, kneFREditSOA, kneConfigObjects, BaseServiceUtils,
  //---ServiceUtils
  MillServiceUtils, CustomersInMarketsServiceUtils ;

{$R *.dfm}

{ TFRAMElistPlafonds }

constructor TFRAMEagentsCustomers.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'Code=agent';
  DataPacketName := 'AgentCustomer';
  PropertyName := 'customers';
  FrameType := frtDetail;

  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
//    kneUtils.TkneGeneric.SplitString('marketName;', ReadOnlyFields, ';', True);
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    HiddenFields.Clear;
    kneUtils.TkneGeneric.SplitString('agent;', HiddenFields, ';', True);
    // Ordem Campos ............................................................
//    OrderFields.Clear;
    DefineOrderFields('stat; mill; customer; customerName; commRate; commType; lastUpd; updBy;');
//    kneUtils.TkneGeneric.SplitString('seqNum', OrderFields, ';', True);
    // Key Fields ..............................................................
    KeyFields:= 'lastUpd;agent;mill;customer';
    // Custom Editors ..........................................................
```

#### **FRagentsCustomers.dfm**

```
inherited FRAMEagentsCustomers: TFRAMEagentsCustomers
  Font.Name = 'Verdana'
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTfindCustomer: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxEDTcommRate: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
      Properties.MaskKind = emkRegExprEx
      Properties.EditMask = '\d{1,2}(\.\d{1,2})?'
      Properties.MaxLength = 0
    end
    object cxICBcommType: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <>
    end
  end
end
```
<!-- tabs:end -->

