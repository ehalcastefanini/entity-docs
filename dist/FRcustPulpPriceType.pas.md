<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustPulpPriceType`

## 1. Overview:

### Objective and Problem Solved:
The `FRcustPulpPriceType` code snippet defines a Delphi frame (`TFRAMEcustPulpPriceType`) that provides a user interface for managing and displaying information related to PIX (likely a pricing or financial data type). It includes fields for selecting PIX type, date, currency, and exchange rate type, along with a checkbox for enabling/disabling PIX price. The frame is designed to interact with a database and external services for currency selection.

This frame solves the problem of providing a structured and interactive UI for managing PIX-related data, ensuring proper validation and interaction with external services.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., `CurrencyServiceUtils`).
- **Database Components**: For binding UI elements to database fields.
- **Custom Components**: Includes custom labels, panels, and combo boxes (`TsLabel`, `TcxDBImageComboBox`, etc.).

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `ICBOpixType`: Dropdown (Image ComboBox) for selecting PIX type.
  - `ICBOpixDate`: Dropdown (Image ComboBox) for selecting PIX date.
  - `FRAMEFindCurrency`: Custom component for selecting currency.
  - `CHBpixPrice`: Checkbox for enabling/disabling PIX price.
  - Labels (`TsLabel`): For describing the fields.
- **Form Actions and Effects**:
  - `CHBpixPriceClick`: Toggles the state of PIX price.
  - `ShowData`: Populates the form with data from the database.
  - `m_SetFindCurrency`: Configures the currency selection component.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can:
  - Select a PIX type, date, and currency.
  - Enable or disable the PIX price using a checkbox.
  - View and interact with pre-populated data from the database.
- Software can:
  - Validate the form data before submission.
  - Interact with external services for currency selection.

### Main Components:
- **Labels (`TsLabel`)**: Display field descriptions.
- **Dropdowns (`TcxDBImageComboBox`)**: Allow users to select PIX type, date, and exchange rate type.
- **Checkbox (`TsDBCheckBox`)**: Toggles the PIX price.
- **Custom Component (`TFRAMEFindEditSOA`)**: Handles currency selection with external service integration.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHBpixPrice`:
  ```pseudo
  if checkbox clicked then
    toggle PIX price state
  ```
- `ShowData` method:
  ```pseudo
  if form is loaded then
    populate fields with database values
  ```
- `m_SetFindCurrency` method:
  ```pseudo
  configure currency selection component with database and service settings
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created and initialized (`Create` method).
   - The `m_SetFindCurrency` method configures the currency selection component.
   - The action panel is hidden, and available actions are cleared.
2. **User Interaction**:
   - Users interact with the form elements (e.g., dropdowns, checkbox).
   - Events like `CHBpixPriceClick` are triggered based on user actions.
3. **Data Handling**:
   - The `ShowData` method populates the form with data from the database.
   - External services are used for currency selection.

### Required Data:
- Database fields:
  - `pixType`
  - `pixDate`
  - `pixCurrency`
- External service data for currency selection.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Checkbox (`CHBpixPrice`)**:
  - Action: Toggles the PIX price state.
  - Preconditions: None.
- **Dropdowns (`ICBOpixType`, `ICBOpixDate`, `ICBOpixExchange`)**:
  - Action: Allow selection of values.
  - Preconditions: Must be populated with valid options.
- **Currency Selection (`FRAMEFindCurrency`)**:
  - Action: Opens a dialog for selecting currency.
  - Preconditions: Requires a valid database connection and service configuration.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- `ShowActionPanel`: Default is `False`.
- `AvailableActions`: Default is an empty string.

### Field Validation and Conditions:
- `pixCurrency`: Must be a valid currency code.
- Other validations are not explicitly defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`Create`**:
   - Initializes the frame and its components.
   - Configures the action panel and currency selection.
2. **`m_SetFindCurrency`**:
   - Configures the currency selection component with database and service settings.
3. **`ShowData`**:
   - Populates the form with data from the database.

---

## 6. API Service Consumption:

### External Service:
- **Service Name**: `CurrencyServiceUtils`
- **Purpose**: Provides currency selection functionality.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Currency code and description.
- **Data Received**: List of available currencies.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `FRAMEFindCurrency`
  - **Condition**: Enabled only when properly configured in `m_SetFindCurrency`.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components**: For interacting with external services.
- **Database Components**: For binding UI elements to database fields.

### Custom Components:
- `TFRAMEFindEditSOA`: Handles currency selection.
- `TsLabel`, `TcxDBImageComboBox`, `TsDBCheckBox`: Custom UI components.

---

## 9. Fields and Validations Listing:

### Fields:
1. **PIX Type** (`pixType`):
   - Type: Dropdown (Image ComboBox).
   - Required: Yes.
2. **PIX Date** (`pixDate`):
   - Type: Dropdown (Image ComboBox).
   - Required: Yes.
3. **PIX Currency** (`pixCurrency`):
   - Type: Custom component.
   - Required: Yes.
4. **PIX Price**:
   - Type: Checkbox.
   - Required: No.

### Mapping:
- `pixType` → Database column: `pixType`
- `pixDate` → Database column: `pixDate`
- `pixCurrency` → Database column: `pixCurrency`

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Configure Currency Selection] --> [Load Data] --> [User Interaction] --> [Save/Submit Data]
```

### Sequence Diagram:
```plaintext
User --> Form: Interact with fields
Form --> Database: Fetch data
Form --> Service: Fetch currency options
```

### Code Snippets:
```delphi
procedure TFRAMEcustPulpPriceType.m_SetFindCurrency;
begin
  with FRAMEfindCurrency do
  begin
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'pixCurrency';
    FindDialog.Caption := 'Currency Selection';
    FindDialog.ProviderService := TCurrencyServiceUtils.Create(FindDialog);
  end;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- `// SET DAS PROPRIEDADES DA FRAME`: Indicates frame properties are being set.
- `// configura�ao da Find Edit`: Configures the currency selection component.

---

## 12. Conclusion:

The `FRcustPulpPriceType` frame provides a structured UI for managing PIX-related data. It integrates with a database and external services for currency selection. While it is functional, the lack of explicit error handling and validation could be improved.

---

## 13. Short Summary:

The `FRcustPulpPriceType` frame is a Delphi-based UI for managing PIX data, integrating database fields and external services for currency selection. It supports dropdowns, checkboxes, and validation logic for user interaction.#### **FRcustPulpPriceType.pas**

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

