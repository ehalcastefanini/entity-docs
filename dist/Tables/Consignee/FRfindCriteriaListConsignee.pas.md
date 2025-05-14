<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é criar um formulário para a busca de critérios relacionados a consignatários (consignees). Ele permite que o usuário insira e filtre informações como código, descrição, país, mercado de consignação, destino, moinho, código postal e status. Este formulário é utilizado para facilitar a busca e filtragem de dados em sistemas que lidam com informações de consignatários.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TsLabel`, `TsEdit`, `TcxImageComboBox` e frames personalizados (`TFRAMEFindEditSOA`).
  - Serviços auxiliares como `ConsigneeMarketServiceUtils`, `CountryServiceUtils` e `MillServiceUtils`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `EDTcode` (Campo de texto para código).
      - `EDTdescription` (Campo de texto para descrição).
      - `FRAMEfindCountry` (Frame para busca de país).
      - `FRAMEfindConsMarket` (Frame para busca de mercado de consignação).
      - `EDTpostal` (Campo de texto para código postal).
      - `FRAMEfindDestination` (Frame para busca de destino).
      - `FRAMEfindMill` (Frame para busca de moinho).
      - `CBOstat` (ComboBox para seleção de status).
    - **Ações do Formulário e seus Efeitos:**
      - Preenchimento dos campos para definir critérios de busca.
      - Validação dos campos obrigatórios antes de executar a busca.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - O usuário pode preencher os campos do formulário para definir critérios de busca.
  - O sistema valida os campos obrigatórios antes de processar os critérios.

* **Componentes Principais:**
  - Campos de entrada (`TsEdit`, `TcxImageComboBox`).
  - Frames personalizados para busca (`TFRAMEFindEditSOA`).
  - Labels (`TsLabel`) para descrever os campos.

* **Pseudo-código das Ações e Eventos:**
  - `OnClick` de um botão de busca: `se botão clicado então validar campos e executar busca`.
  - `OnChange` de um campo: `se valor do campo alterado então validar campo`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário com o método `Initialize`.
  - Configuração dos frames de busca com métodos como `m_SetFindCountry`, `m_SetFindConsMarket`, etc.
  - O usuário preenche os campos e interage com o formulário.
  - A busca é executada após validação dos critérios.

* **Dados Necessários:**
  - Código, descrição, país, mercado de consignação, destino, moinho, código postal e status.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - A busca só pode ser executada se os campos obrigatórios (código, descrição, país) estiverem preenchidos.

* **Filtros Disponíveis:**
  - País, mercado de consignação, destino, moinho, status.

* **Mensagens de Erro:**
  - "Field Code must be defined" se o campo código não for preenchido.
  - "Field Description must be defined" se o campo descrição não for preenchido.
  - "Field Country must be defined" se o campo país não for preenchido.

* **Valores Padrão dos Campos:**
  - `CBOstat`: Primeiro item da lista é selecionado por padrão.

* **Validação de Campos:**
  - `EDTcode`: Deve ser preenchido em letras maiúsculas.
  - `EDTdescription`: Deve ser preenchido.
  - `CBOstat`: Deve ter um valor selecionado.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Initialize`: Configura o formulário e inicializa os frames de busca.
  - `GetCriteriaValues`: Retorna os critérios de busca definidos pelo usuário.
  - Métodos como `m_SetFindCountry`, `m_SetFindConsMarket`, etc., configuram os frames de busca.

---

## 6. Consumo de Serviços de API:

* Não há chamadas explícitas a serviços de API no código fornecido, mas os utilitários importados (`ConsigneeMarketServiceUtils`, `CountryServiceUtils`, `MillServiceUtils`) podem ser usados para interagir com APIs externas.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código fornecido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `kneFGFindUtils` para utilitários gerais.
  - `ConsigneeMarketServiceUtils`, `CountryServiceUtils`, `MillServiceUtils` para interações com serviços relacionados.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Frame para busca de dados.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Código (`EDTcode`): Tipo texto, obrigatório, letras maiúsculas.
  - Descrição (`EDTdescription`): Tipo texto, obrigatório.
  - País (`FRAMEfindCountry`): Tipo frame, obrigatório.
  - Mercado de Consignação (`FRAMEfindConsMarket`): Tipo frame, opcional.
  - Código Postal (`EDTpostal`): Tipo texto, opcional.
  - Destino (`FRAMEfindDestination`): Tipo frame, opcional.
  - Moinho (`FRAMEfindMill`): Tipo frame, opcional.
  - Status (`CBOstat`): Tipo combo box, obrigatório.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Frame: TFRAMEfindCriteriaListConsignee;
  begin
    Frame := TFRAMEfindCriteriaListConsignee.Create(Self);
    Frame.Initialize;
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 749px; height: 190px; font-family: Verdana;">
    <label for="code">Code:</label>
    <input id="code" type="text" style="text-transform: uppercase;" />
    <label for="description">Name:</label>
    <input id="description" type="text" />
    <label for="country">Country:</label>
    <input id="country" type="text" />
    <label for="consMarket">Cons. Market:</label>
    <input id="consMarket" type="text" />
    <label for="postal">Postal:</label>
    <input id="postal" type="text" />
    <label for="destination">Destination:</label>
    <input id="destination" type="text" />
    <label for="mill">Mill:</label>
    <input id="mill" type="text" />
    <label for="status">Status:</label>
    <select id="status">
      <option>Active</option>
      <option>Inactive</option>
    </select>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `//#24509 (cmosilva 12-08-2021)`: Define que o primeiro item do combo box de status é selecionado por padrão.

---

## 12. Conclusão:

O código fornece um formulário robusto para busca de critérios relacionados a consignatários. Ele é bem estruturado, mas depende de frames e serviços externos para funcionalidade completa. Uma limitação é a ausência de validações mais detalhadas e mensagens de erro amigáveis.

---

## 13. Resumo Curto:

Formulário em Delphi para busca de consignatários, permitindo filtrar por código, descrição, país, mercado, destino, moinho, postal e status. Utiliza frames personalizados e validações básicas para garantir critérios de busca adequados.#### **FRfindCriteriaListConsignee.pas**

```
unit FRfindCriteriaListConsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, kneFRFindEditSOA, Mask,
  DBCtrls, sDBEdit, StdCtrls, sEdit, sLabel, kneConfigObjects, sCheckBox,
  cxGraphics, cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxImageComboBox;

type
  TFRAMEfindCriteriaListConsignee = class(TFRAMEfindCriteria)
    LBL4: TsLabel;
    LBL5: TsLabel;
    LBLcode: TsLabel;
    EDTcode: TsEdit;
    sLabel1: TsLabel;
    EDTdescription: TsEdit;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindConsMarket: TFRAMEFindEditSOA;
    LBLpostal: TsLabel;
    EDTpostal: TsEdit;
    LBLmill: TsLabel;
    LBLdest: TsLabel;
    FRAMEfindDestination: TFRAMEFindEditSOA;
    FRAMEfindMill: TFRAMEFindEditSOA;
    LBLstat: TsLabel;
    CBOstat: TcxImageComboBox;
  private
    FFieldConsMarket: string;
    FFieldCode: string;
    FFieldDescription: string;
    FFieldCountry: string;
    FFieldPostDescrip: string;
    FFieldDestination: string;
    FFieldMill: string;

    procedure m_SetFindCountry;
    procedure m_SetFindConsMarket;
    procedure SetFieldCode(const Value: string);
    procedure SetFieldConsMarket(const Value: string);
    procedure SetFieldCountry(const Value: string);
    procedure SetFieldDescription(const Value: string);
    procedure m_SetFindDestination;
    procedure m_SetFindMill;
    procedure SetFieldDestination(const Value: string);
    procedure SetFieldMill(const Value: string);
    procedure SetFieldPostDescrip(const Value: string);
    { Private declarations }
  protected
    function GetCriteriaValues: TArrayOfFieldCriteria; override;
  public
    { Public declarations }
    property FieldCode: string read FFieldCode write SetFieldCode;
    property FieldDescription: string read FFieldDescription write SetFieldDescription;
    property FieldCountry: string read FFieldCountry write SetFieldCountry;
    property FieldConsMarket: string read FFieldConsMarket write SetFieldConsMarket;
    property FieldMill:string read FFieldMill write SetFieldMill;
    property FieldDestination:string read FFieldDestination write SetFieldDestination;
    property FieldPostDescrip:string read FFieldPostDescrip write SetFieldPostDescrip;

    procedure Initialize; override;
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEfindCriteriaListConsignee: TFRAMEfindCriteriaListConsignee;

implementation

uses
  kneUtils, kneFGFindUtils,
  ConsigneeMarketServiceUtils, CountryServiceUtils, MillServiceUtils;
{$R *.dfm}

{ TFRAMEfindCriteriaConsignee }

constructor TFRAMEfindCriteriaListConsignee.Create(AOwner: TComponent);
begin
  inherited;
  m_SetFindCountry;
  m_SetFindConsMarket;
  m_SetFindDestination;
  m_SetFindMill;

  //#24509 (cmosilva 12-08-2021)
  if CBOstat.Properties.Items.Count > 0 then
    CBOstat.ItemIndex := 0;

end;

function TFRAMEfindCriteriaListConsignee.GetCriteriaValues: TArrayOfFieldCriteria;
begin
  Result := nil;

  Assert(FFieldCode <> '', 'Field Code must be defined');
  Assert(FFieldDescription <> '', 'Field Description must be defined');
  Assert(FieldCountry <> '', 'Field Country must be defined');
```

#### **FRfindCriteriaListConsignee.dfm**

```
inherited FRAMEfindCriteriaListConsignee: TFRAMEfindCriteriaListConsignee
  Width = 749
  Height = 190
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  ParentFont = False
  object LBL4: TsLabel [0]
    Left = 7
    Top = 65
    Width = 55
    Height = 13
    Caption = 'Co&untry: '
    FocusControl = FRAMEfindCountry.FE
  end
  object LBL5: TsLabel [1]
    Left = 8
    Top = 39
    Width = 85
    Height = 13
    Caption = 'Cons. Mar&ket: '
    FocusControl = FRAMEfindConsMarket.FE
  end
  object LBLcode: TsLabel [2]
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'Co&de:'
    FocusControl = EDTcode
  end
  object sLabel1: TsLabel [3]
    Left = 183
    Top = 13
    Width = 38
    Height = 13
    Caption = 'N&ame:'
    FocusControl = EDTdescription
  end
  object LBLpostal: TsLabel [4]
    Left = 8
    Top = 117
    Width = 39
    Height = 13
    Caption = 'Pos&tal:'
    FocusControl = EDTpostal
  end
  object LBLmill: TsLabel [5]
    Left = 183
    Top = 91
    Width = 69
    Height = 13
    Caption = 'D&estination:'
    FocusControl = FRAMEfindDestination.FE
  end
  object LBLdest: TsLabel [6]
    Left = 8
    Top = 91
    Width = 23
    Height = 13
    Caption = 'M&ill:'
    FocusControl = FRAMEfindMill.FE
  end
  object LBLstat: TsLabel [7]
    Left = 487
    Top = 117
    Width = 28
    Height = 13
    Caption = 'Stat:'
    FocusControl = CBOstat
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object EDTcode: TsEdit [8]
    Left = 91
    Top = 8
    Width = 86
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    TabOrder = 0
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  object EDTdescription: TsEdit [9]
    Left = 242
    Top = 8
    Width = 381
    Height = 21
```
<!-- tabs:end -->

