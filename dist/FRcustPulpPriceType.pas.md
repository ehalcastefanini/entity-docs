<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um formulário para gerenciar informações relacionadas a preços de polpa (Pulp Price Type) em um sistema. Ele permite a visualização, configuração e validação de dados relacionados a tipos de preços, datas, moedas e taxas de câmbio.

* **Tecnologias Utilizadas:**  
  - Delphi (VCL Framework).
  - Componentes visuais como `TcxDBImageComboBox`, `TsLabel`, `TsDBCheckBox`, entre outros.
  - Integração com serviços SOAP para manipulação de dados de moeda.

* **Tipo de Formulário:**  
  Este é um formulário com os seguintes elementos:
  - **Elementos do Formulário:**
    - `ICBOpixType` (ComboBox para selecionar o tipo de preço).
    - `ICBOpixDate` (ComboBox para selecionar a data de cálculo).
    - `FRAMEFindCurrency` (Campo de busca para selecionar a moeda).
    - `CHBpixPrice` (Checkbox para ativar/desativar o preço).
    - `ICBOpixExchange` (ComboBox para selecionar o tipo de taxa de câmbio).
  - **Ações do Formulário:**
    - Configuração de campos e validação de dados.
    - Integração com serviços externos para busca de informações de moeda.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Configurar e validar os campos do formulário.
  - Selecionar valores para tipo de preço, data, moeda e taxa de câmbio.
  - Ativar/desativar o campo de preço.

* **Componentes Principais:**
  - `TFRAMEcustPulpPriceType`: Classe principal que gerencia o formulário.
  - `FRAMEFindCurrency`: Componente para busca de moedas.
  - `ICBOpixType`, `ICBOpixDate`, `ICBOpixExchange`: Comboboxes para seleção de valores.
  - `CHBpixPrice`: Checkbox para ativar/desativar o preço.

* **Pseudo-código de Ações e Eventos:**
  - `CHBpixPriceClick`:  
    ```pseudo
    if checkbox clicked then
        toggle price field visibility or state
    ```
  - `ShowData`:  
    ```pseudo
    if form loaded then
        populate fields with metadata values
    ```
  - `m_SetFindCurrency`:  
    ```pseudo
    configure currency search field with data source and provider service
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`Create`):
     - Configurações iniciais, como visibilidade de painéis e ações disponíveis.
     - Configuração do campo de busca de moeda.
  2. Interação do usuário:
     - Seleção de valores nos comboboxes.
     - Clique no checkbox para ativar/desativar o preço.
  3. Validação e exibição de dados:
     - Validação dos campos ao salvar.
     - Exibição de dados com base na metadata.

* **Dados Necessários:**
  - Tipo de preço (`pixType`).
  - Data de cálculo (`pixDate`).
  - Moeda (`pixCurrency`).
  - Tipo de taxa de câmbio (`pixExchange`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão de salvar só deve ser habilitado se todos os campos obrigatórios forem preenchidos corretamente.
  - O campo de busca de moeda só é configurado após a inicialização do formulário.

* **Filtros Disponíveis:**
  - Filtros para seleção de tipo de preço, data, moeda e tipo de taxa de câmbio.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não atender aos critérios esperados.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validação de Campos:**
  - `pixCurrency`: Deve ser validado com base no código da moeda.
  - `pixType`, `pixDate`, `pixExchange`: Devem ser preenchidos com valores válidos.

---

## 5. Funções Principais:

* **`Create`:**  
  Configura o formulário, define propriedades iniciais e configura o campo de busca de moeda.

* **`m_SetFindCurrency`:**  
  Configura o campo de busca de moeda com o serviço de dados e define as opções de seleção.

* **`ShowData`:**  
  Preenche os campos do formulário com valores provenientes da metadata.

* **`CHBpixPriceClick`:**  
  Gerencia a ativação/desativação do campo de preço com base no estado do checkbox.

---

## 6. Consumo de Serviços de API:

* **Serviço Externo:**  
  - **Nome do Serviço:** CurrencyServiceUtils.  
  - **Finalidade:** Buscar informações de moeda para preenchimento do campo de busca.  
  - **Dados Enviados:** Não especificado no código.  
  - **Dados Recebidos:** Informações de moeda (código e descrição).  
  - **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo de busca de moeda (`FRAMEFindCurrency`) é configurado apenas após a inicialização do formulário.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `InvokeRegistry`, `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `kneFRCtrlEditSOA`, `kneFRFindEditSOA`: Componentes personalizados para edição e busca.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Componente para busca de dados.
  - `TCurrencyServiceUtils`: Classe utilitária para manipulação de dados de moeda.

---

## 9. Listagem de Campos e Validações:

* **Campos do Formulário:**
  - `pixType` (ComboBox, obrigatório, valores definidos pela metadata).
  - `pixDate` (ComboBox, obrigatório, valores definidos pela metadata).
  - `pixCurrency` (Campo de busca, obrigatório, validado pelo código da moeda).
  - `pixExchange` (ComboBox, obrigatório, valores definidos pela metadata).
  - `pixPrice` (Checkbox, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `pixType` → `pixType` (coluna no banco de dados).
  - `pixDate` → `pixDate`.
  - `pixCurrency` → `pixCurrency`.
  - `pixExchange` → `pixExchange`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  FRAMEcustPulpPriceType := TFRAMEcustPulpPriceType.Create(Self);
  FRAMEcustPulpPriceType.ShowData;
  ```

* **HTML Representando o Formulário:**
  ```html
  <div style="width: 1275px; font-family: Verdana;">
    <label for="pixType">PIX Type:</label>
    <select id="pixType" style="width: 300px;"></select>
    <br>
    <label for="pixDate">PIX Date to Calc:</label>
    <select id="pixDate" style="width: 300px;"></select>
    <br>
    <label for="pixCurrency">PIX Currency:</label>
    <input type="text" id="pixCurrency" style="width: 300px;">
    <br>
    <label for="pixExchange">Type of Exchange Rate:</label>
    <select id="pixExchange" style="width: 300px;"></select>
    <br>
    <input type="checkbox" id="pixPrice"> PIX Price
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial do formulário no método `Create`.
* Configuração do campo de busca de moeda no método `m_SetFindCurrency`.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar informações de preços de polpa, com integração a serviços externos para busca de dados de moeda. No entanto, faltam detalhes sobre validações e tratamento de erros, o que pode limitar sua robustez.

---

## 13. Resumo Curto:

Formulário para gerenciar preços de polpa, permitindo configuração de tipo, data, moeda e taxa de câmbio, com integração a serviços SOAP para busca de dados.#### **FRcustPulpPriceType.pas**

```
unit FRcustPulpPriceType;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRStatusInfo, cxDBEdit, sDBEdit, sLabel, sFrameAdapter, sBitBtn,
  sPanel, cxGraphics, kneFRFindEditSOA, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, sCheckBox,
  sDBCheckBox, sButton, cxCheckBox;

type
  TFRAMEcustPulpPriceType = class(TFRAMEBaseCtrlEditSOA)
    LBLpixType: TsLabel;
    ICBOpixType: TcxDBImageComboBox;
    LBLpixDate: TsLabel;
    ICBOpixDate: TcxDBImageComboBox;
    LBLpixCurrency: TsLabel;
    FRAMEFindCurrency: TFRAMEFindEditSOA;
    CHBpixPrice: TsDBCheckBox;
    LBLpixPrice: TsLabel;
    sLabel1: TsLabel;
    ICBOpixExchange: TcxDBImageComboBox;
    procedure ShowData; override;
    procedure CHBpixPriceClick(Sender: TObject);
  private
    { Private declarations }
    procedure m_SetFindCurrency;
    procedure m_SetPixControls(const pv_Sender: TObject);
  protected
    function  m_Validate: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEcustPulpPriceType: TFRAMEcustPulpPriceType;

implementation

uses kneTypes, kneFREditSOA, kneUtils, DMskin, Global, CurrencyServiceUtils;

{$R *.dfm}

{ TFRAMEcustPulpPriceType }

constructor TFRAMEcustPulpPriceType.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  FrameType := frtGhost;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  m_SetFindCurrency;

  FRAMEFindCurrency.Enable := False;
end;

procedure TFRAMEcustPulpPriceType.m_SetFindCurrency;
begin

 with FRAMEfindCurrency do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'pixCurrency';
//    EditSettings.FieldNameForDesc := 'currency';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'currencyCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
//    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

    FindDialog.Caption := 'Currency Selection';

    // instanciar a classe utilit�ria para acesso a dados
    FindDialog.ProviderService := TCurrencyServiceUtils.Create(FindDialog);
  end;

end;

procedure TFRAMEcustPulpPriceType.ShowData;
var
  lv_Field: TField;
  lv_PossibleValues: string;
begin
  inherited;

  // Preenche o controle associado ao campo com os valores vindos da metadata
  with DStable.DataSet do
```

#### **FRcustPulpPriceType.dfm**

```
inherited FRAMEcustPulpPriceType: TFRAMEcustPulpPriceType
  Width = 1275
  Height = 272
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLpixType: TsLabel [0]
    Left = 21
    Top = 56
    Width = 57
    Height = 13
    Caption = 'PIX Type:'
    FocusControl = ICBOpixType
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLpixDate: TsLabel [1]
    Left = 21
    Top = 88
    Width = 100
    Height = 13
    Caption = 'PIX Date to Calc:'
    FocusControl = ICBOpixDate
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLpixCurrency: TsLabel [2]
    Left = 21
    Top = 120
    Width = 82
    Height = 13
    Caption = 'PIX Currency:'
    FocusControl = FRAMEFindCurrency.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLpixPrice: TsLabel [3]
    Left = 25
    Top = 16
    Width = 52
    Height = 13
    Caption = 'PIX Price'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object sLabel1: TsLabel [4]
    Left = 21
    Top = 151
    Width = 137
    Height = 13
    Caption = 'Type of Exchange Rate:'
    FocusControl = ICBOpixExchange
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 238
    Width = 1275
    TabOrder = 5
    Visible = False
    inherited PNLeditActions: TsPanel
      Width = 647
    end
  end
  object ICBOpixType: TcxDBImageComboBox [6]
    Left = 164
    Top = 51
    DataBinding.DataField = 'pixType'
    DataBinding.DataSource = DStable
    Enabled = False
    Properties.Items = <>
    TabOrder = 1
    Width = 300
  end
  object ICBOpixDate: TcxDBImageComboBox [7]
    Left = 164
    Top = 83
    DataBinding.DataField = 'pixDate'
    DataBinding.DataSource = DStable
    Enabled = False
    Properties.Items = <>
```
<!-- tabs:end -->

