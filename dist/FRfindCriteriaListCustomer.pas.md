<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é fornecer uma interface de critérios de busca para listar clientes com base em diferentes parâmetros. Ele permite que os usuários filtrem e pesquisem clientes utilizando critérios como código, nome, mercado, número legal, entre outros. Este componente é útil em sistemas onde é necessário realizar buscas detalhadas e refinadas de clientes.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do componente.
  - Componentes visuais como `TsPageControl`, `TsEdit`, `TsLabel`, `TsComboBox`, e `TFRAMEFindEditSOA` para a interface do usuário.
  - Frameworks e bibliotecas como `cxGraphics`, `cxControls`, `cxContainer`, `cxEdit` para suporte a controles avançados.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - Campos de texto (`TsEdit`) para entrada de dados como código, descrição, número legal, cidade.
      - Comboboxes (`TsComboBox`, `TcxImageComboBox`) para seleção de unidades de negócios, canais de negócios, status e origem do cliente.
      - Checkboxes (`TsCheckBox`) para opções booleanas como "Verificar VAT".
      - Labels (`TsLabel`) para descrever os campos.
      - Abas (`TsPageControl` e `TsTabSheet`) para organizar os critérios e informações adicionais.
    - **Ações do Formulário e seus Efeitos:**
      - Validação dos critérios preenchidos.
      - Inicialização e configuração de critérios específicos.
      - Busca de clientes com base nos critérios preenchidos.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Os usuários podem preencher os campos de critérios e realizar buscas.
  - O sistema valida os critérios preenchidos antes de executar a busca.
  - Configuração de critérios específicos como mercado, grupo, vendedor, país, entre outros.

* **Componentes Principais:**
  - `TsPageControl` com abas para organizar os critérios.
  - Campos de entrada (`TsEdit`, `TsComboBox`) para os critérios.
  - Métodos privados para configurar critérios específicos.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` de um botão de busca: `se botão clicado então validar critérios e executar busca`.
  - Evento `OnChange` de um campo: `se valor do campo alterado então validar campo`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do componente com o método `Initialize`.
  - Configuração dos critérios específicos com métodos como `m_SetFindCustomerMarket`, `m_SetFindParentCustomer`, etc.
  - Interação do usuário com os campos e execução da busca.

* **Dados Necessários:**
  - Código, nome, mercado, número legal, grupo, vendedor, país, entre outros critérios.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - A busca só pode ser executada se os critérios obrigatórios forem preenchidos.
  - Campos como "Código" e "Nome" devem ser validados antes da busca.

* **Filtros Disponíveis:**
  - Código, Nome, Mercado, Número Legal, Grupo, Vendedor, País, Unidade de Negócio, Canal de Negócio, Status, Origem do Cliente.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Critério inválido" se um critério não atender às validações.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validação de Campos:**
  - Validação de campos como "Código" e "Nome" para garantir que não estejam vazios.
  - Comboboxes devem ter valores válidos selecionados.

---

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `Initialize`: Inicializa o componente e configura os critérios.
  - `Validate`: Valida os critérios preenchidos.
  - `GetCriteriaValues`: Retorna os valores dos critérios preenchidos.

---

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código fornecido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGraphics`, `cxControls`, `cxContainer`, `cxEdit` para suporte a controles avançados.
* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA` para critérios específicos.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - Código (tipo: string, obrigatório).
  - Nome (tipo: string, obrigatório).
  - Mercado (tipo: string, opcional).
  - Número Legal (tipo: string, opcional).
  - Cidade (tipo: string, opcional).
  - Unidade de Negócio (tipo: combobox, opcional).
  - Canal de Negócio (tipo: combobox, opcional).
  - Status (tipo: combobox, opcional).
  - Origem do Cliente (tipo: combobox, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Diagramas:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Frame: TFRAMEfindCriteriaListCustomer;
  begin
    Frame := TFRAMEfindCriteriaListCustomer.Create(Self);
    Frame.Initialize;
    if Frame.Validate then
      ShowMessage('Critérios válidos!');
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 949px; height: 175px; font-family: Verdana;">
    <div style="padding: 10px;">
      <label for="code">Code:</label>
      <input type="text" id="code" style="margin-left: 10px;">
    </div>
    <div style="padding: 10px;">
      <label for="name">Name:</label>
      <input type="text" id="name" style="margin-left: 10px;">
    </div>
    <div style="padding: 10px;">
      <label for="market">Cust. Mkt:</label>
      <input type="text" id="market" style="margin-left: 10px;">
    </div>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Métodos como `m_SetFindCustomerMarket` e `m_SetFindParentCustomer` são essenciais para configurar critérios específicos.

---

## 12. Conclusão:

O código fornece uma interface robusta para busca de clientes com base em critérios detalhados. Sua principal limitação é a ausência de validações explícitas e mensagens de erro detalhadas. No entanto, sua modularidade e organização são pontos fortes.

---

## 13. Resumo Curto:

Componente Delphi para busca de clientes baseado em critérios detalhados, utilizando campos de texto, comboboxes e validações básicas. Ideal para sistemas que exigem filtros refinados e organizados.#### **FRfindCriteriaListCustomer.pas**

```
unit FRfindCriteriaListCustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, StdCtrls, sEdit, sLabel,
  kneFRFindEditSOA, kneUtils, sCheckBox, ComCtrls, sPageControl, Mask,
  DBCtrls, sDBEdit, sComboBox, cxGraphics, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox;

type
  TFRAMEfindCriteriaListCustomer = class(TFRAMEfindCriteria)
    sPageControl1: TsPageControl;
    SHcriteria: TsTabSheet;
    SHadditional: TsTabSheet;
    LBLcode: TsLabel;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sLabel3: TsLabel;
    Label9: TsLabel;
    Label10: TsLabel;
    LBL1: TsLabel;
    EDTcode: TsEdit;
    EDTdescription: TsEdit;
    FRAMEfindCustMarket: TFRAMEFindEditSOA;
    EDTlegalNum: TsEdit;
    FRAMEfindParentCustomer: TFRAMEFindEditSOA;
    FRAMEfindGroup: TFRAMEFindEditSOA;
    FRAMEfindSeller: TFRAMEFindEditSOA;
    FRAMEfindSalesMan: TFRAMEFindEditSOA;
    sLabel4: TsLabel;
    sLabel5: TsLabel;
    LBLoffice: TsLabel;
    FRAMEfindSalesOffice: TFRAMEFindEditSOA;
    sLabel6: TsLabel;
    sLabel7: TsLabel;
    Label3: TsLabel;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    Label21: TsLabel;
    FRAMEfindDeliveryTerms: TFRAMEFindEditSOA;
    Label17: TsLabel;
    FRAMEfindPayment: TFRAMEFindEditSOA;
    Label12: TsLabel;
    FRAMEfindTypeCostumer: TFRAMEFindEditSOA;
    FRAMEfindAgent: TFRAMEFindEditSOA;
    FRAMEFindsalesDir: TFRAMEFindEditSOA;
    EDTcity: TsEdit;
    LBL2: TsLabel;
    LBL3: TsLabel;
    FRAMEFindMill: TFRAMEFindEditSOA;
    CBObusUnit: TsComboBox;
    CHKverifyVat: TsCheckBox;
    sLabel8: TsLabel;
    CBObusChannel: TcxImageComboBox;
    LBLstat: TsLabel;
    CBOstat: TcxImageComboBox;
    sLabel9: TsLabel;
    CBOcustOrig: TsComboBox;
  private
    FBusUnit: string;
    FUsrBusUnitDefault: string;
    { Private declarations }
    procedure m_SetFindCustomerMarket;
    procedure m_SetFindParentCustomer;
    procedure m_SetFindGroup;
    procedure m_SetFindSeller;
    procedure m_SetFindAgent;
    procedure m_SetFindOffice;
    procedure m_SetFindSalesMan;
    procedure m_SetFindCountry;
    procedure m_SetFindTypeCostumer;
    procedure m_SetFindPayment;
    procedure m_SetFindDeliveryTerms;
    procedure m_SetFindSalesDir;
    procedure m_SetFindMill;
    procedure SetBusUnit(const Value: string);
    procedure SetUsrBusUnitDefault(const Value: string);
    procedure GetAllBusChannels;
  protected
    function GetCriteriaValues: TArrayOfFieldCriteria; override;
  public
    { Public declarations }
    procedure Initialize; override;
    constructor Create(AOwner: TComponent); override;
    function Validate: boolean;

  published
    property BusUnit : string read FBusUnit write SetBusUnit;
    property UsrBusUnitDefault: string read FUsrBusUnitDefault write SetUsrBusUnitDefault;

  end;

var
  FRAMEfindCriteriaListCustomer: TFRAMEfindCriteriaListCustomer;

implementation

uses
  //--- LB
```

#### **FRfindCriteriaListCustomer.dfm**

```
inherited FRAMEfindCriteriaListCustomer: TFRAMEfindCriteriaListCustomer
  Width = 949
  Height = 175
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  ParentFont = False
  object sPageControl1: TsPageControl [0]
    Left = 0
    Top = 0
    Width = 949
    Height = 175
    ActivePage = SHcriteria
    Align = alClient
    TabOrder = 0
    SkinData.SkinSection = 'PAGECONTROL'
    object SHcriteria: TsTabSheet
      Caption = 'Criteria'
      SkinData.CustomColor = False
      SkinData.CustomFont = False
      object LBLcode: TsLabel
        Left = 8
        Top = 13
        Width = 35
        Height = 13
        Caption = 'C&ode:'
        FocusControl = EDTcode
        ParentFont = False
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
      end
      object sLabel1: TsLabel
        Left = 196
        Top = 13
        Width = 38
        Height = 13
        Caption = 'Name:'
        FocusControl = EDTdescription
        ParentFont = False
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
      end
      object sLabel2: TsLabel
        Left = 8
        Top = 39
        Width = 59
        Height = 13
        Caption = 'C&ust. Mkt:'
        FocusControl = FRAMEfindCustMarket.FE
        ParentFont = False
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
      end
      object sLabel3: TsLabel
        Left = 406
        Top = 65
        Width = 65
        Height = 13
        Caption = 'Legal Num:'
        FocusControl = EDTlegalNum
        ParentFont = False
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
      end
      object Label9: TsLabel
        Left = 406
        Top = 39
        Width = 76
        Height = 13
        Caption = 'Parent Cust.:'
        ParentFont = False
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Height = -11
        Font.Name = 'Verdana'
        Font.Style = []
      end
      object Label10: TsLabel
        Left = 8
        Top = 65
        Width = 40
        Height = 13
        Caption = '&Group:'
        FocusControl = FRAMEfindGroup.FE
        ParentFont = False
        Font.Charset = ANSI_CHARSET
        Font.Color = 5059883
        Font.Height = -11
        Font.Name = 'Verdana'
```
<!-- tabs:end -->

