<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é fornecer uma interface para a seleção de critérios de busca relacionados a países e datas de eventos. Ele permite que o usuário selecione um país e um ano, além de ativar ou desativar a busca por uma data específica de evento. Este componente é útil em sistemas que precisam filtrar dados com base em critérios específicos, como relatórios ou consultas em bancos de dados.

* **Tecnologias Utilizadas:**
  - Delphi (VCL - Visual Component Library).
  - Componentes de terceiros, como `TsLabel`, `TsCheckBox`, `TcxDateEdit`, `TsComboBox`, e `TFRAMEFindEditSOA`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `CHKeventDate` (Checkbox): Permite ativar/desativar a busca por data de evento.
      - `DTEeventDate` (DateEdit): Campo para selecionar uma data de evento.
      - `FRAMEfindCountry` (Custom Frame): Campo para selecionar um país.
      - `CBOyear` (ComboBox): Lista de anos para seleção.
    - **Ações do Formulário e seus Efeitos:**
      - Seleção de um país ou ano atualiza os critérios de busca.
      - Ativação do checkbox `CHKeventDate` habilita o campo de data.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - O usuário pode:
    - Selecionar um país.
    - Selecionar um ano.
    - Ativar/desativar a busca por data de evento.
    - Escolher uma data de evento, caso o checkbox esteja ativado.

* **Componentes Principais:**
  - `CHKeventDate`: Controla a ativação do campo de data.
  - `DTEeventDate`: Permite a seleção de uma data.
  - `FRAMEfindCountry`: Permite a seleção de um país.
  - `CBOyear`: Lista de anos para seleção.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do checkbox:
    ```pseudo
    se checkbox for clicado então
      habilitar ou desabilitar o campo de data
    ```
  - Evento de inicialização:
    ```pseudo
    ao inicializar o formulário
      preencher a lista de anos
      definir a data atual no campo de data
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização:
    - O formulário é carregado.
    - A lista de anos é preenchida com um intervalo de 5 anos antes e depois do ano atual.
    - A data atual é definida no campo de data.
  - Interações do Usuário:
    - O usuário pode ativar o checkbox para habilitar o campo de data.
    - O usuário pode selecionar um país e um ano.

* **Dados Necessários:**
  - País (opcional).
  - Ano (obrigatório).
  - Data de evento (opcional, se o checkbox estiver ativado).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O campo de data só é habilitado se o checkbox `CHKeventDate` estiver marcado.
  - A busca só será realizada se pelo menos um critério for preenchido.

* **Filtros Disponíveis:**
  - País.
  - Ano.
  - Data de evento (opcional).

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas no código fornecido.

* **Valores Padrão dos Campos:**
  - `DTEeventDate`: Data atual.
  - `CBOyear`: Ano atual.

* **Validações e Condições dos Campos:**
  - `DTEeventDate`: Deve conter uma data válida.
  - `CBOyear`: Deve conter um ano válido.

---

## 5. Funções Principais:

* **`Create`:**
  - Preenche a lista de anos e define a data atual no campo de data.

* **`GetCriteriaValues`:**
  - Retorna os critérios de busca selecionados pelo usuário.

* **`CHKeventDateClick`:**
  - Habilita ou desabilita o campo de data com base no estado do checkbox.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo `DTEeventDate` só é habilitado se o checkbox `CHKeventDate` estiver marcado.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `TsLabel`, `TsCheckBox`, `TcxDateEdit`, `TsComboBox`: Componentes visuais.
  - `TFRAMEFindEditSOA`: Componente customizado para seleção de países.

* **Componentes Customizados:**
  - `TFRAMEFindEditSOA`: Usado para selecionar um país.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `CHKeventDate` (Checkbox, opcional): Ativa/desativa a busca por data.
  - `DTEeventDate` (DateEdit, opcional): Data de evento.
  - `FRAMEfindCountry` (Custom Frame, opcional): País.
  - `CBOyear` (ComboBox, obrigatório): Ano.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `eventDate` → `DTEeventDate`.
  - `countryCode` → `FRAMEfindCountry`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEfindCriteriaCountryCal := TFRAMEfindCriteriaCountryCal.Create(Self);
  FRAMEfindCriteriaCountryCal.Initialize;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 509px;">
    <label for="country">Country:</label>
    <input type="text" id="country" style="width: 381px;" />
    <label for="year">Year:</label>
    <select id="year" style="width: 65px;">
      <option>2023</option>
      <option>2024</option>
    </select>
    <input type="checkbox" id="eventDate" />
    <label for="eventDate">Event Date:</label>
    <input type="date" id="eventDateField" disabled />
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* O método `Create` inicializa os critérios de busca e preenche a lista de anos.
* O método `GetCriteriaValues` constrói os critérios de busca com base nos campos preenchidos.

---

## 12. Conclusão:

O código fornece uma interface funcional para a seleção de critérios de busca relacionados a países e datas de eventos. Ele é bem estruturado e fácil de usar, mas poderia ser melhorado com a adição de mensagens de erro e validações mais robustas.

---

## 13. Resumo Curto:

Este componente Delphi permite a seleção de critérios de busca por país, ano e data de evento, com inicialização automática de valores padrão e lógica condicional para habilitação de campos. Ideal para sistemas de consulta e relatórios.#### **FRfindCriteriaCountryCal.pas**

```
unit FRfindCriteriaCountryCal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, StdCtrls, sCheckBox, Mask,
  sMaskEdit, sCustomComboEdit, sTooledit, sLabel, kneFRFindEditSOA,
  kneConfigObjects, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxCalendar, dxCntner, dxEditor, dxExEdtr,
  dxEdLib, sComboBox;

type
  TFRAMEfindCriteriaCountryCal = class(TFRAMEfindCriteria)
    sLabel3: TsLabel;
    CHKeventDate: TsCheckBox;
    DTEeventDate: TcxDateEdit;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    LBLcode: TsLabel;
    CBOyear: TsComboBox;
    procedure CHKeventDateClick(Sender: TObject);
  private
    { Private declarations }
    procedure m_SetFindCountry;
  protected
    function GetCriteriaValues: TArrayOfFieldCriteria; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Initialize;

  end;

var
  FRAMEfindCriteriaCountryCal: TFRAMEfindCriteriaCountryCal;

implementation

uses
  DateUtils, kneUtils, CountryServiceUtils;

{$R *.dfm}

{ TFRAMEfindCriteriaCountryCal }

constructor TFRAMEfindCriteriaCountryCal.Create(AOwner: TComponent);
var
  lv_Year, lv_Month, lv_Day: Word;
  lv_i: Integer;
begin
  inherited;

  // Inicializa��o de crit�rios
  DecodeDate(Date, lv_Year, lv_Month, lv_Day);
  // Preenche a combo box CBOyear
  //TkneControls.fg_ffg_FillComboBoxWithYears(CBOyear, lv_Year);
  CBOyear.Clear;
  for lv_i := lv_Year - 5 to lv_Year + 5 do
  begin
    CBOyear.Items.Add(IntToStr(lv_i));
  end;
  CBOyear.ItemIndex := 5;
  m_SetFindCountry;

  // Atribui a data de hoje ao controlo de datas
  DTEeventDate.Date := Today;

end;

function TFRAMEfindCriteriaCountryCal.GetCriteriaValues: TArrayOfFieldCriteria;
begin
  Result := nil;

  // Event Date
  if CHKeventDate.Checked then
  begin
    if DTEeventDate.EditValue <> null then
    begin
      // adicionar novo elemento
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := TFieldCriteria.Create;

      Result[High(Result)].logical  := 'AND'; // Operador l�gico: {AND,OR}
      Result[High(Result)].field    := 'eventDate'; // nome do campo do dataset associado
      Result[High(Result)].operator := '='; // Operador a usar: {<, <=, =, >=, >, <>, LIKE}
      Result[High(Result)].value    :=
        DateTimeToStr(DTEeventDate.Date, kneEnv.ServiceFormatSettings); // valor
    end;
  end;

  // Description
  if FRAMEfindCountry.Text <> '' then
  begin
    // adicionar novo elemento
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TFieldCriteria.Create;

    Result[High(Result)].logical  := 'AND'; // Operador l�gico: {AND,OR}
    Result[High(Result)].field    := 'countryCode'; // nome do campo do dataset associado
```

#### **FRfindCriteriaCountryCal.dfm**

```
inherited FRAMEfindCriteriaCountryCal: TFRAMEfindCriteriaCountryCal
  Width = 509
  object sLabel3: TsLabel [0]
    Left = 16
    Top = 39
    Width = 39
    Height = 13
    Caption = '&Country:'
    FocusControl = FRAMEfindCountry.FE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object LBLcode: TsLabel [1]
    Left = 208
    Top = 13
    Width = 25
    Height = 13
    Caption = '&Year:'
    FocusControl = CBOyear
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object CHKeventDate: TsCheckBox [2]
    Left = 16
    Top = 11
    Width = 82
    Height = 19
    Caption = 'Event Date:'
    TabOrder = 0
    OnClick = CHKeventDateClick
    SkinData.SkinSection = 'CHECKBOX'
    ImgChecked = 0
    ImgUnchecked = 0
  end
  object DTEeventDate: TcxDateEdit [3]
    Left = 104
    Top = 8
    Enabled = False
    ParentFont = False
    Properties.InputKind = ikMask
    Style.StyleController = DMODskin.cxEditStyles1
    TabOrder = 1
    Width = 85
  end
  inline FRAMEfindCountry: TFRAMEFindEditSOA [4]
    Left = 104
    Top = 34
    Width = 381
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    inherited PNLdesc: TPanel
      Left = 85
      Width = 296
      inherited DBEDesc: TsDBEdit
        Width = 296
      end
      inherited EDDesc: TsEdit
        Width = 296
      end
    end
    inherited PNLcode: TPanel
      Width = 85
      inherited DBE: TsDBEdit
        Width = 64
      end
      inherited FE: TsMaskEdit
        Width = 64
      end
      inherited PNLbutton: TPanel
        Left = 64
      end
    end
  end
  object CBOyear: TsComboBox
    Left = 240
    Top = 8
    Width = 65
    Height = 21
    Alignment = taLeftJustify
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
```
<!-- tabs:end -->

