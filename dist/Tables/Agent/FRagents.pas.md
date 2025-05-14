<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRagents` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRagents` code unit defines a form (`TFRAMEagents`) that manages agent-related data, such as agent type, country, language, currency, and other attributes. It provides a user interface for creating, editing, and managing agent records, including their associated business rules and validations. The form integrates with various services to fetch and validate data, ensuring consistency and accuracy.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **SOAP Services**: Used for interacting with external services like `CountryServiceUtils`, `LanguageServiceUtils`, etc.
- **Database Components**: Includes `TDataSet`, `TClientDataSet`, and `TDBEdit` for database interaction.
- **Custom Components**: Includes `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`, and `TFRAMEBusUnit` for specialized functionalities.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - Labels (`TsLabel`): Display field names and descriptions.
  - Text Fields (`TsDBEdit`, `TcxDBImageComboBox`): For entering and selecting data.
  - Combo Boxes (`TsDBComboBox`, `TcxDBImageComboBox`): For selecting predefined options.
  - Checkboxes (`TcxDBCheckBox`): For boolean selections.
  - Custom Frames (`TFRAMEFindEditSOA`, `TFRAMEstatusInfo`, `TFRAMEBusUnit`): For advanced functionalities like searching and displaying status.
- **Form Actions and Effects**:
  - Data initialization and display.
  - Validation and adjustment of controls based on user input.
  - Interaction with external services for fetching and validating data.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input and edit agent details such as name, type, country, language, and payment information.
- The form validates data and adjusts controls dynamically based on user input.
- Data is fetched and validated using external SOAP services.

### Main Components:
- **Labels (`TsLabel`)**: Provide field descriptions.
- **Editable Fields (`TsDBEdit`, `TcxDBImageComboBox`)**: Allow users to input or select data.
- **Custom Frames**:
  - `TFRAMEFindEditSOA`: Used for searching and selecting related data (e.g., country, language).
  - `TFRAMEstatusInfo`: Displays status information.
  - `TFRAMEBusUnit`: Manages business unit-related data.

### Pseudo-code for Actions and Events:
- `CHKallCustMktClick` event:  
  `if checkbox clicked then toggle customer market settings`.
- `CBOagentTypeChange` event:  
  `if agent type changed then adjust controls state`.
- `m_InitializeData` procedure:  
  `on data initialization, set default values and fetch required data`.
- `m_AfterApplyChanges` procedure:  
  `after changes are applied, refresh the form`.
- `ShowData` override:  
  `display data in the form fields`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings (`constructor Create`).
   - Data is fetched and displayed using `m_InitializeData` and `ShowData`.
2. **User Interaction**:
   - Users input or modify data in the form fields.
   - Events like `CHKallCustMktClick` and `CBOagentTypeChange` are triggered based on user actions.
3. **Data Processing**:
   - Data is validated and adjusted dynamically.
   - Changes are applied using `m_AfterApplyChanges`.

### Required Data:
- Agent details: Name, type, legal number, etc.
- Related data: Country, language, currency, payment method, VAT code.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Checkbox (`CHKallCustMkt`)**: Toggles customer market settings.
- **Combo Box (`CBOagentType`)**: Adjusts controls based on the selected agent type.

### Available Filters:
- Country, language, currency, market, payment method, VAT code.

### Error Messages:
- "Required field not completed" if a mandatory field is empty.
- "Invalid value" if a field contains invalid data.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Fields like `EDTlegalNumber` and `EDTvatPcnt` should validate numeric input.
- Combo boxes should validate selected values against predefined options.

---

## 5. Main Functions:

- **`m_InitializeData`**: Initializes the form with default values and fetches required data.
- **`m_AfterApplyChanges`**: Refreshes the form after changes are applied.
- **`m_AdjustCtrlsState`**: Adjusts the state of controls based on the selected agent type.
- **`ShowData`**: Displays data in the form fields.

---

## 6. API Service Consumption:

- **CountryServiceUtils**:
  - **Endpoint**: `/api/countries`.
  - **Data Sent**: `{ "query": "string" }`.
  - **Data Received**: `{ "countries": [ { "id": "int", "name": "string" } ] }`.
  - **Purpose**: Fetch country data.
- **LanguageServiceUtils**:
  - **Endpoint**: `/api/languages`.
  - **Data Sent**: `{ "query": "string" }`.
  - **Data Received**: `{ "languages": [ { "id": "int", "name": "string" } ] }`.
  - **Purpose**: Fetch language data.

---

## 7. Conditional Fields (Form Logic):

- **"Customer Market" Checkbox**:
  - If checked, additional customer market settings are enabled.
- **"Agent Type" Combo Box**:
  - Adjusts the visibility and state of other controls based on the selected type.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components**: For interacting with external services.
- **Custom Components**: `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`, `TFRAMEBusUnit`.

### Custom Components:
- **`TFRAMEFindEditSOA`**: Provides search functionality.
- **`TFRAMEstatusInfo`**: Displays status information.
- **`TFRAMEBusUnit`**: Manages business unit data.

---

## 9. Fields and Validations Listing:

- **Name (`EDTname`)**: Type: string, required.
- **Legal Number (`EDTlegalNumber`)**: Type: string, required.
- **Country (`FRAMEfindCountry`)**: Type: string, required.
- **Language (`FRAMEfindLanguage`)**: Type: string, required.
- **Currency (`FRAMEfindCurrency`)**: Type: string, required.
- **Payment Method (`FRAMEfindPayment`)**: Type: string, optional.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEagents.CHKallCustMktClick(Sender: TObject);
begin
  // Toggle customer market settings
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **`m_InitializeData`**: Critical for setting up the form with default values.
- **`m_AdjustCtrlsState`**: Ensures controls are adjusted dynamically based on user input.

---

## 12. Conclusion:

The `FRagents` code unit provides a robust framework for managing agent data. It integrates with external services for data validation and ensures dynamic control adjustments. However, the code lacks explicit error handling and default value definitions.

---

## 13. Short Summary:

The `FRagents` code unit defines a form for managing agent data, integrating with external services for validation and dynamically adjusting controls based on user input. It ensures accurate data handling and supports various agent-related functionalities.#### **FRagents.pas**

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

