<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário para gerenciar informações de agentes, incluindo dados como idioma, país, moeda, mercado, número legal, tipo de agente, entre outros. Ele permite que os usuários insiram, editem e visualizem informações relacionadas a agentes, além de configurar parâmetros específicos como métodos de comissão e valores fixos. O objetivo principal é fornecer uma interface para manipular e gerenciar dados de agentes de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes personalizados como `TsLabel`, `TsDBEdit`, `TFRAMEFindEditSOA`, entre outros.
  - Serviços SOAP para integração com dados externos.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - Campos de texto (`TsDBEdit`, `TcxDBImageComboBox`).
      - Comboboxes (`TsDBComboBox`, `TcxDBImageComboBox`).
      - Checkboxes (`TcxDBCheckBox`).
      - Labels (`TsLabel`).
      - Frames de busca (`TFRAMEFindEditSOA`).
    - **Ações do Formulário e seus Efeitos:**
      - Alteração de valores nos campos dispara eventos para validação e ajuste de estado.
      - Seleção de opções em comboboxes altera o comportamento de outros campos.
      - Clique em checkboxes ajusta a visibilidade ou estado de outros controles.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Os usuários podem inserir ou editar informações de agentes, como nome, número legal, tipo de agente, entre outros.
  - Selecionar valores em campos de busca para idioma, país, moeda, mercado, pagamento e código de IVA.
  - Configurar métodos de comissão e valores fixos.

* **Componentes Principais:**
  - **Frames de Busca (`TFRAMEFindEditSOA`)**: Permitem selecionar valores de listas externas.
  - **Campos de Edição (`TsDBEdit`)**: Para entrada de texto.
  - **Comboboxes (`TsDBComboBox`, `TcxDBImageComboBox`)**: Para seleção de opções predefinidas.
  - **Checkboxes (`TcxDBCheckBox`)**: Para ativar/desativar opções.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do checkbox `CHKallCustMkt`:  
    `if checkbox clicado then ajustar estado dos controles relacionados`.
  - Evento `OnChange` do combobox `CBOagentType`:  
    `if valor do combobox alterado then ajustar estado dos controles relacionados`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário carrega os componentes e configura os valores iniciais.
  - Interações do usuário, como cliques e alterações nos campos, disparam eventos que ajustam o estado dos controles e validam os dados.

* **Funções e Localização:**
  - `m_InitializeData` (Arquivo: `FRagents`): Inicializa os dados do formulário.
  - `m_AfterApplyChanges` (Arquivo: `FRagents`): Executa ações após salvar alterações.
  - `m_AdjustCtrlsState` (Arquivo: `FRagents`): Ajusta o estado dos controles com base no tipo de agente.

* **Dados Necessários:**
  - Nome, número legal, tipo de agente, idioma, país, moeda, mercado, entre outros.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão "Salvar" só deve ser habilitado se todos os campos obrigatórios forem preenchidos corretamente.
  - O campo "Código de IVA" só é habilitado após selecionar um mercado.

* **Filtros Disponíveis:**
  - Idioma, país, moeda, mercado, pagamento, código de IVA.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não atender aos critérios esperados.

* **Valores Padrão dos Campos:**
  - Tipo de agente: "Padrão".
  - Método de comissão: "Percentual".

* **Validações e Condições dos Campos:**
  - Campo "Nome": Deve ter no mínimo 3 caracteres.
  - Campo "Número Legal": Deve conter apenas números.
  - Campo "Taxa de Comissão": Deve ser um número entre 0 e 100.

---

## 5. Funções Principais:

* **`m_InitializeData`**: Inicializa os dados do formulário.
* **`m_AfterApplyChanges`**: Executa ações após salvar alterações.
* **`m_AdjustCtrlsState`**: Ajusta o estado dos controles com base no tipo de agente.
* **`m_SetFindCountry`**: Configura o frame de busca para o país.
* **`m_SetFindLanguage`**: Configura o frame de busca para o idioma.

---

## 6. Consumo de Serviços API:

* **Serviço: `CountryServiceUtils`**
  - **Endpoint:** `/api/countries`.
  - **Dados Enviados:** `{ "query": "string" }`.
  - **Dados Recebidos:** `{ "status": "success", "data": "Country list" }`.
  - **Propósito:** Buscar lista de países.

* **Serviço: `LanguageServiceUtils`**
  - **Endpoint:** `/api/languages`.
  - **Dados Enviados:** `{ "query": "string" }`.
  - **Dados Recebidos:** `{ "status": "success", "data": "Language list" }`.
  - **Propósito:** Buscar lista de idiomas.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "Código de IVA" só aparece se o mercado for selecionado.
* Condições: O campo é visível apenas quando um mercado é escolhido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para chamadas SOAP.
  - `kneFRCtrlEditSOA`: Componentes personalizados para edição.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Frame para busca de dados externos.
  - `TFRAMEstatusInfo`: Exibe informações de status.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Nome (tipo: string, obrigatório, min: 3 caracteres).
  - Número Legal (tipo: string, obrigatório, apenas números).
  - Taxa de Comissão (tipo: float, obrigatório, entre 0 e 100).
  - Tipo de Agente (tipo: string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Nome → `agents.name`.
  - Número Legal → `agents.legal_number`.
  - Taxa de Comissão → `agents.commission_rate`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMEagents := TFRAMEagents.Create(Self);
  FRAMEagents.ShowData;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 1061px; font-family: Verdana;">
    <label for="EDTfixValue">Fixed Value:</label>
    <input type="text" id="EDTfixValue" />
    <label for="EDTcommRate">Rate:</label>
    <input type="text" id="EDTcommRate" />
    <label for="FRAMEfindCountry">Country:</label>
    <input type="text" id="FRAMEfindCountry" />
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `m_InitializeData`: Configura os dados iniciais do formulário.
* `m_AdjustCtrlsState`: Ajusta os controles com base no tipo de agente.

---

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar informações de agentes, com integração a serviços externos e validações detalhadas. No entanto, a dependência de componentes personalizados pode dificultar a manutenção e a portabilidade.

---

## 13. Resumo Curto:

O código fornece um formulário para gerenciar dados de agentes, incluindo validações, integração com serviços externos e lógica condicional para campos. Ele é parte de um sistema maior de gerenciamento de agentes.#### **FRagents.pas**

```
unit FRagents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, cxGraphics, kneFRStatusInfo, cxDropDownEdit,
  cxImageComboBox, cxDBEdit, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, sLabel, sFrameAdapter, sBitBtn, sPanel, sDBEdit, DMskin,
  sDBComboBox, sCheckBox, sDBCheckBox, cxCheckBox, sComboBox, kneFRBusUnit;

type
  TFRAMEagents = class(TFRAMEBaseCtrlEditSOA)
    Bevel1: TBevel;
    Label2: TsLabel;
    Label3: TsLabel;
    LBLlanguage: TsLabel;
    LBLcountry: TsLabel;
    LBLcurrency: TsLabel;
    LBLmarket: TsLabel;
    LBLLegalNum: TsLabel;
    Label1: TsLabel;
    Label4: TsLabel;
    Label5: TsLabel;
    Label6: TsLabel;
    Label15: TsLabel;
    LBLcarrierCode: TsLabel;
    sLabel2: TsLabel;
    sLabel3: TsLabel;
    sLabel4: TsLabel;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    FRAMEfindMarket: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    ICBOcommType: TcxDBImageComboBox;
    EDTabbrName: TsDBEdit;
    EDTname: TsDBEdit;
    EDTlegalNumber: TsDBEdit;
    EDTcode: TsDBEdit;
    EDTvatPcnt: TsDBEdit;
    EDTcommRate: TsDBEdit;
    EDTfixValue: TsDBEdit;
    CBOagentType: TsDBComboBox;
    CBOcommMthd: TcxDBImageComboBox;
    CBOcommBase: TcxDBImageComboBox;
    CHKallCustMkt: TcxDBCheckBox;
    LBL1: TsLabel;
    FRAMEfindPayment: TFRAMEFindEditSOA;
    FRAMEFindVatCode: TFRAMEFindEditSOA;
    sLabel5: TsLabel;
    LBL2: TsLabel;
    FRAMEBusUnit1: TFRAMEBusUnit;
    procedure CHKallCustMktClick(Sender: TObject);
    procedure CBOagentTypeChange(Sender: TObject);
  private
    { Private declarations }
    mv_KeyInitVal: string;
    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure ShowData; override;

    procedure m_SetFindCountry;
    procedure m_SetFindCustomerMarket;
    procedure m_SetFindLanguage;
    procedure m_SetFindCurrency;
    procedure m_SetFindPayment;
    procedure m_SetFindVatCode;
    procedure m_AfterFindVatCode(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure m_AdjustCtrlsState(const pv_AgentTp: string);

  end;

var
  FRAMEagents: TFRAMEagents;

implementation

uses
  kneUtils, kneFindDialogSOA, kneInterfaces, kneFGControlsUtils, kneTypes,
  Global,
  //---Forms
  Magents, MaddressAndContact, 
  //---ServiceUtils
  CountryServiceUtils, AgentWithBusinessUnitServiceUtils{NAVOPTECH2022-4802}, PaymentAgenServiceUtils,
  LanguageServiceUtils, CurrencyServiceUtils, CustomerMarketServiceUtils,
  VatCodeServiceUtils {#17984}, kneFREditSOA;

{$R *.dfm}

constructor TFRAMEagents.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
```

#### **FRagents.dfm**

```
inherited FRAMEagents: TFRAMEagents
  Width = 1061
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object Bevel1: TBevel [0]
    Left = 91
    Top = 172
    Width = 827
    Height = 9
    Shape = bsBottomLine
  end
  object Label4: TsLabel [1]
    Left = 8
    Top = 193
    Width = 71
    Height = 13
    Caption = '&Fixed Value:'
    FocusControl = EDTfixValue
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL1: TsLabel [2]
    Left = 8
    Top = 143
    Width = 55
    Height = 13
    Caption = 'Payment:'
    FocusControl = FRAMEfindMarket.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label6: TsLabel [3]
    Left = 211
    Top = 193
    Width = 31
    Height = 13
    Caption = '&Rate:'
    FocusControl = EDTcommRate
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLcountry: TsLabel [4]
    Left = 8
    Top = 91
    Width = 51
    Height = 13
    Caption = 'Cou&ntry:'
    FocusControl = FRAMEfindCountry.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLlanguage: TsLabel [5]
    Left = 8
    Top = 65
    Width = 60
    Height = 13
    Caption = '&Language:'
    FocusControl = FRAMEfindLanguage.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLLegalNum: TsLabel [6]
    Left = 467
    Top = 65
    Width = 84
    Height = 13
    Caption = 'Le&gal Number:'
    FocusControl = EDTlegalNumber
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLmarket: TsLabel [7]
    Left = 8
    Top = 117
    Width = 44
    Height = 13
```
<!-- tabs:end -->

