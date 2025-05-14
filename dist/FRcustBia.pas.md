<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário para gerenciar informações relacionadas a "Customer Bia" (provavelmente um tipo de configuração ou dados específicos de clientes). Ele permite a entrada, edição e visualização de dados como períodos, porcentagens de vendas, dias de referência, tolerância e rebates (descontos). O objetivo é fornecer uma interface para manipular esses dados de forma estruturada e eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes visuais como `TsLabel`, `TsDBEdit`, `TcxDBMaskEdit`, `TcxDBImageComboBox`.
  - Manipulação de banco de dados com `TDataSet` e `TClientDataSet`.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - Campos de entrada (`TsDBEdit`, `TcxDBMaskEdit`) para valores numéricos e texto.
      - Combobox (`TcxDBImageComboBox`) para seleção de modos.
      - Labels (`TsLabel`) para descrever os campos.
    - **Ações do Formulário e seus Efeitos:**
      - Inserção de novos registros.
      - Configuração de valores padrão para novos registros.
      - Validação e estilização de campos.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Inserir novos registros no banco de dados.
  - Configurar valores padrão para novos registros.
  - Exibir e editar informações relacionadas a períodos, porcentagens e rebates.

* **Componentes Principais:**
  - `TFRAMEcustBia`: Classe principal que define o formulário.
  - `TsDBEdit`, `TcxDBMaskEdit`, `TcxDBImageComboBox`: Componentes para entrada de dados.
  - `TsLabel`: Labels para descrever os campos.
  - `CDStable`: Dataset para manipulação de dados.

* **Tradução para Pseudo-código:**
  - Evento `OnCreate`:
    ```pseudo
    Ao criar o formulário:
      Configurar propriedades como campos principais, nome do pacote de dados e tipo de frame.
      Configurar visibilidade do painel de ações.
      Aplicar estilos aos campos de entrada.
    ```
  - Evento `CDStableNewRecord`:
    ```pseudo
    Ao criar um novo registro:
      Inicializar valores padrão para os campos.
      Definir o ano e mês atual no formato "YYYYMM".
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`Create`):
     - Configura propriedades e estilos.
  2. Interação do usuário:
     - Preenchimento de campos e seleção de opções.
  3. Inserção de novos registros:
     - Valores padrão são aplicados automaticamente.

* **Dados Necessários:**
  - Período inicial e final.
  - Porcentagens de vendas (estoque e direto).
  - Dias de referência e tolerância.
  - Valores de rebates (All, CutSize, Folio, Reels).
  - Modo de operação.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Inserir novo registro: Requer que os campos obrigatórios sejam preenchidos.
  - Configurar valores padrão: Executado automaticamente ao criar um novo registro.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Ano e mês atual no formato "YYYYMM" para campos relacionados a períodos.

* **Validação e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`Create`:**
  - Configura propriedades do formulário e aplica estilos aos campos.

* **`CDStableNewRecord`:**
  - Inicializa valores padrão para novos registros.

* **`m_InsertNewRecord`:**
  - Método público para inserir um novo registro (detalhes não fornecidos no código).

* **`SetForDOCADDR`:**
  - Método público para configurar algo relacionado a "DOCADDR" (detalhes não fornecidos no código).

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos definidas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneFRCtrlEditSOA`, `InvokeRegistry`, `SOAPHTTPClient`: Provavelmente usados para integração com serviços SOAP.
  - `DMskin`: Usado para aplicar estilos aos campos.

* **Componentes Personalizados:**
  - `TFRAMEBaseCtrlEditSOA`: Classe base para o formulário.
  - `TFRAMEstatusInfo`: Componente para exibir informações de status.

---

## 9. Listagem de Campos e Validações:

* **Campos do Formulário:**
  - `EDTstockSalesPerc` (tipo: numérico, obrigatório, não definido no código).
  - `EDTreferenceDays` (tipo: numérico, obrigatório, não definido no código).
  - `EDTdirectSalesPerc` (tipo: numérico, obrigatório, não definido no código).
  - `EDTtoleranceDays` (tipo: numérico, obrigatório, não definido no código).
  - `EDTperiodoIni` (tipo: data, obrigatório, não definido no código).
  - `EDTperiodoFim` (tipo: data, obrigatório, não definido no código).
  - `EDTrebAll`, `EDTrebCutSize`, `EDTrebFolio`, `EDTrebReels` (tipo: numérico, obrigatório, não definido no código).
  - `ICBOmode` (tipo: combobox, obrigatório, não definido no código).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    Frame: TFRAMEcustBia;
  begin
    Frame := TFRAMEcustBia.Create(Self);
    Frame.m_InsertNewRecord;
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 791px; font-family: Verdana;">
    <label style="display: block;">Period:</label>
    <input type="text" style="width: 100px;" />
    <label style="display: block;">To:</label>
    <input type="text" style="width: 100px;" />
    <label style="display: block;">Stock Sales:</label>
    <input type="text" style="width: 100px;" />
    <label style="display: block;">Direct Sales:</label>
    <input type="text" style="width: 100px;" />
    <!-- Outros campos omitidos para brevidade -->
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades no método `Create`.
* Inicialização de valores padrão no método `CDStableNewRecord`.

---

## 12. Conclusão:

O código implementa um formulário funcional para gerenciar dados de "Customer Bia". Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Sua integração com banco de dados e uso de componentes personalizados são pontos fortes.

---

## 13. Resumo Curto:

Formulário Delphi para gerenciar dados de "Customer Bia", incluindo períodos, porcentagens e rebates. Permite inserção e edição de registros com valores padrão configurados automaticamente.#### **FRcustBia.pas**

```
unit FRcustBia;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRStatusInfo, cxDBEdit, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxCalendar, sFrameAdapter, sBitBtn, sPanel,
  sDBEdit, sLabel, cxGraphics, cxImageComboBox, sBevel;

type
  TFRAMEcustBia = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBLInicialPeriod: TsLabel;
    LBLEndPeriod: TsLabel;
    Label1: TsLabel;
    Label2: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    EDTstockSalesPerc: TsDBEdit;
    EDTreferenceDays: TsDBEdit;
    EDTdirectSalesPerc: TsDBEdit;
    EDTtoleranceDays: TsDBEdit;
    EDTperiodoIni: TsDBEdit;
    EDTperiodoFim: TsDBEdit;
    LBLrebate: TsLabel;
    LBLall: TsLabel;
    LBLcutSize: TsLabel;
    LBLfolio: TsLabel;
    LBLreels: TsLabel;
    EDTrebAll: TcxDBMaskEdit;
    EDTrebCutSize: TcxDBMaskEdit;
    EDTrebFolio: TcxDBMaskEdit;
    EDTrebReels: TcxDBMaskEdit;
    BVL1: TsBevel;
    LBL4: TsLabel;
    ICBOmode: TcxDBImageComboBox;
    procedure CDStableNewRecord(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure m_InsertNewRecord;      
    procedure SetForDOCADDR;
  end;

var
  FRAMEcustBia: TFRAMEcustBia;

implementation

uses kneTypes, kneFREditSOA, kneUtils, DMskin;

{$R *.dfm}

{ TFRAMEcustBia }

constructor TFRAMEcustBia.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerBia';
  PropertyName := 'customerBia';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
//  FRAMEstatusInfo1.DataSource := DStable;  
  EDTrebAll.Style.StyleController :=  DMODskin.cxEditStyles1;
  ICBOmode.Style.StyleController :=  DMODskin.cxEditStyles1;
  EDTrebCutSize.Style.StyleController :=  DMODskin.cxEditStyles1;
  EDTrebFolio.Style.StyleController :=  DMODskin.cxEditStyles1;
  EDTrebReels.Style.StyleController :=  DMODskin.cxEditStyles1;
end;

procedure TFRAMEcustBia.CDStableNewRecord(DataSet: TDataSet);
var
  lv_Year, lv_Month, lv_Day: Word;
  lv_YearMonth: string;
begin
  inherited;

  // Aplica os valores por default definidos na metadata
  TkneDB.InitializeFieldDefaults(CDStable);

  DecodeDate(Now, lv_Year, lv_Month, lv_Day);

  lv_YearMonth := IntToStr(lv_Year) + FormatFloat('0#',lv_Month);//IntToStr(lv_Month);

```

#### **FRcustBia.dfm**

```
inherited FRAMEcustBia: TFRAMEcustBia
  Width = 791
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLInicialPeriod: TsLabel [0]
    Left = 8
    Top = 16
    Width = 41
    Height = 13
    Caption = 'Period:'
  end
  object LBLEndPeriod: TsLabel [1]
    Left = 166
    Top = 16
    Width = 19
    Height = 13
    Caption = 'To:'
  end
  object Label1: TsLabel [2]
    Left = 8
    Top = 39
    Width = 72
    Height = 13
    Caption = 'Stock Sales:'
    FocusControl = EDTstockSalesPerc
  end
  object Label2: TsLabel [3]
    Left = 166
    Top = 39
    Width = 74
    Height = 13
    Caption = 'Direct Sales:'
    FocusControl = EDTdirectSalesPerc
  end
  object Label3: TsLabel [4]
    Left = 326
    Top = 39
    Width = 61
    Height = 13
    Caption = 'Ref. Days:'
    FocusControl = EDTreferenceDays
  end
  object Label4: TsLabel [5]
    Left = 476
    Top = 39
    Width = 94
    Height = 13
    Caption = 'Tolerance Days:'
    FocusControl = EDTreferenceDays
  end
  object LBLrebate: TsLabel [6]
    Left = 8
    Top = 65
    Width = 46
    Height = 13
    Caption = 'Rebates'
  end
  object LBLall: TsLabel [7]
    Left = 42
    Top = 117
    Width = 19
    Height = 13
    Caption = 'All:'
  end
  object LBLcutSize: TsLabel [8]
    Left = 168
    Top = 117
    Width = 49
    Height = 13
    Caption = 'CutSize:'
  end
  object LBLfolio: TsLabel [9]
    Left = 294
    Top = 117
    Width = 31
    Height = 13
    Caption = 'Folio:'
  end
  object LBLreels: TsLabel [10]
    Left = 400
    Top = 117
    Width = 36
    Height = 13
    Caption = 'Reels:'
  end
  object BVL1: TsBevel [11]
    Left = 71
    Top = 72
    Width = 565
    Height = 3
    Shape = bsTopLine
  end
  object LBL4: TsLabel [12]
    Left = 42
    Top = 91
    Width = 35
    Height = 13
    Caption = 'Mode:'
  end
  inherited PNLfooter: TsPanel
```
<!-- tabs:end -->

