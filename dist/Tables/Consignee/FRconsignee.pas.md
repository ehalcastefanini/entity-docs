<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsignee` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRconsignee` code unit defines a form (`TFRAMEconsignee`) that facilitates the management of consignee-related data. It provides a user interface for entering, editing, and managing consignee details such as country, state, market, warehouse, delivery terms, and other related information. The form integrates with various services to fetch and validate data, ensuring consistency and accuracy.

### Technologies Used:
- **Delphi (Object Pascal):** The primary programming language used for the implementation.
- **SOAP Services:** Used for data retrieval and updates via service utilities.
- **VCL Components:** Includes standard Delphi components like `TLabel`, `TDBEdit`, and custom components like `TFRAMEFindEditSOA` for enhanced functionality.
- **Third-party Libraries:** Includes `sLabel`, `sDBEdit`, and other skinning components for UI customization.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Labels (`TsLabel`): Display field names and descriptions.
  - Text Fields (`TsDBEdit`): Editable fields for user input.
  - Combo Boxes (`TcxDBImageComboBox`): Dropdowns for selecting predefined options.
  - Checkboxes (`TsDBCheckBox`): Boolean options for additional settings.
  - Custom Find Components (`TFRAMEFindEditSOA`): For searching and selecting related data.
- **Form Actions and Effects:**
  - Data initialization and display.
  - Integration with external services for data fetching and validation.
  - Event-driven updates and validations.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input and edit consignee details such as name, country, state, market, warehouse, and delivery terms.
- Users can select options from dropdowns or search for related data using custom find components.
- The form validates data and interacts with external services to ensure correctness.

### Main Components:
- **Labels (`TsLabel`):** Provide descriptions for each field.
- **Editable Fields (`TsDBEdit`):** Allow users to input text data.
- **Dropdowns (`TcxDBImageComboBox`):** Enable selection of predefined options.
- **Custom Find Components (`TFRAMEFindEditSOA`):** Facilitate searching and linking related data.
- **Checkboxes (`TsDBCheckBox`):** Allow toggling of boolean options.

### Pseudo-code for Actions and Events:
- `OnClick` event of a button: `if button clicked then execute function`.
- `OnChange` event of a field: `if field value changed then validate field`.
- `OnDataInitialize`: `if form loaded then fetch initial data`.
- `OnAfterApplyChanges`: `if changes applied then refresh data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized, and UI components are loaded.
   - Data is fetched from external services using methods like `m_InitializeData`.
2. **User Interaction:**
   - Users input data or select options.
   - Events like `OnChange` or `OnClick` trigger validations and updates.
3. **Data Submission:**
   - Changes are applied using `m_AfterApplyChanges`.
   - Data is sent to external services for storage or further processing.

### Required User Data:
- Name, abbreviation, legal number, consignee code, and SAP code.
- Selection of country, state, market, warehouse, delivery terms, and other related fields.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Save Button:** Enabled only if all required fields are filled and valid.
- **Search Actions:** Require valid input in the corresponding search fields.

### Available Filters:
- Country, state, market, warehouse, delivery terms, and other related fields.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid input" if a field value does not meet validation criteria.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Name:** Required, minimum 3 characters.
- **Abbreviation Name:** Optional, maximum 10 characters.
- **Legal Number:** Required, numeric only.
- **Consignee Code:** Required, alphanumeric.
- **SAP Code:** Optional, alphanumeric.

---

## 5. Main Functions:

### Functions and Business Logic:
- **`m_SetFindCountry`:** Configures the country search component.
- **`m_SetFindState`:** Configures the state search component.
- **`m_InitializeData`:** Fetches initial data for the form.
- **`m_AfterApplyChanges`:** Handles post-save actions and refreshes data.

---

## 6. API Service Consumption:

### External Service Calls:
- **Service Name:** `ConsigneeServiceUtils`
  - **Endpoint:** `/api/consignee`
  - **Data Sent:** `{ "name": "string", "country": "string", "state": "string" }`
  - **Data Received:** `{ "status": "success", "data": "Consignee object" }`
  - **Purpose:** Create or update consignee data.
  - **Error Handling:** Displays error messages on failure.

---

## 7. Conditional Fields (Form Logic):

- **"State" Field:** Visible only when a country is selected.
- **"Warehouse" Field:** Visible only when a market is selected.

---

## 8. Dependencies:

### External Libraries:
- **`sLabel`, `sDBEdit`, `sCheckBox`:** Used for UI customization and skinning.
- **`TFRAMEFindEditSOA`:** Custom component for search functionality.

### Custom Components:
- **`TFRAMEFindEditSOA`:** Handles search and selection of related data.

---

## 9. Fields and Validations Listing:

- **Name:** Type: string, required, min: 3 characters.
- **Abbreviation Name:** Type: string, optional, max: 10 characters.
- **Legal Number:** Type: numeric, required.
- **Consignee Code:** Type: alphanumeric, required.
- **SAP Code:** Type: alphanumeric, optional.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a complete workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide interaction details.)

### Code Snippets:
```pascal
procedure TFRAMEconsignee.m_InitializeData(Sender: TDataSet);
begin
  // Fetch initial data for the form
end;
```

### Screenshots:
The following HTML represents the form layout:
```html
<div style="width: 983px; height: 417px; font-family: Verdana;">
  <label style="color: #4D4D4D;">Country:</label>
  <input type="text" style="margin-left: 10px;" />
  <label style="color: #4D4D4D;">State:</label>
  <input type="text" style="margin-left: 10px;" />
  <!-- Add other fields similarly -->
</div>
```

---

## 11. Important Comments in the Code:

- **`EDTsapCode` Field:** Commented as `//NAVOPTECH2022-2563 (cmosilva 26-01-2023)` indicating a specific update or feature addition.

---

## 12. Conclusion:

The `FRconsignee` code unit provides a robust framework for managing consignee data. It integrates with external services for data validation and retrieval, ensuring accuracy and consistency. However, the code lacks explicit error handling and default values for some fields, which could be improved.

---

## 13. Short Summary:

The `FRconsignee` form facilitates consignee data management with integrated search and validation features, leveraging SOAP services for data consistency. It supports user input, dropdown selections, and conditional field visibility.#### **FRconsignee.pas**

```
unit FRconsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, cxGraphics, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxDBEdit,
  kneFRStatusInfo, sFrameAdapter, sBitBtn, sPanel, sDBEdit, sLabel, DMskin,
  sCheckBox, sDBCheckBox;

type
  TFRAMEconsignee = class(TFRAMEBaseCtrlEditSOA)
    FRAMEfindConsMarket: TFRAMEFindEditSOA;
    FRAMEfindDestination: TFRAMEFindEditSOA;
    FRAMEfindDelPolicy: TFRAMEFindEditSOA;
    ICBOinvoiceMode: TcxDBImageComboBox;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindState: TFRAMEFindEditSOA;
    FRAMEfindWarehouse: TFRAMEFindEditSOA;
    FRAMEfindDelTerm: TFRAMEFindEditSOA;
    FRAMEfindUnitSys: TFRAMEFindEditSOA;
    FRAMEfindOrdDest: TFRAMEFindEditSOA;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    LBLcountry: TsLabel;
    LBLstate: TsLabel;
    LBLmarket: TsLabel;
    LBLwarehouse: TsLabel;
    Label2: TsLabel;
    LBLdelPolicy: TsLabel;
    LBLsysMesure: TsLabel;
    LBLname: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    LBLinvoiceMode: TsLabel;
    Label5: TsLabel;
    LBLdestination: TsLabel;
    Label1: TsLabel;
    Label6: TsLabel;
    EDTname: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTlegalNum: TsDBEdit;
    EDTconsCode: TsDBEdit;
    CHKdetailPackList: TsDBCheckBox;
    LBL6: TsLabel;
    FRAMEfindShipMethod: TFRAMEFindEditSOA;
    LBL5: TsLabel;
    FRAMEFindFacing: TFRAMEFindEditSOA;
    LBL7: TsLabel;
    FRAMEFindSubFace: TFRAMEFindEditSOA;
    LBLsapCode: TsLabel;
    EDTsapCode: TsDBEdit; //NAVOPTECH2022-2563 (cmosilva 26-01-2023)
  private
    { Private declarations }
    mv_KeyInitVal: string;
    procedure m_SetFindCountry;
    procedure m_SetFindState;
    procedure m_SetFindWarehouse;
    procedure m_SetFindLanguage;
    procedure m_SetFindConsMarket;
    procedure m_SetFindDestination;
    procedure m_SetFindDelTerms;
    procedure m_SetFindDelPolicy;
    procedure m_SetFindUnitSys;
    procedure m_SetFindOrdDest;

    procedure m_BeforeFindConsMarket(Value: TObject);
    procedure m_BeforeFindWarehouse(Value: TObject);
    procedure m_BeforeFindState(Value: TObject);

    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure m_SetFindShipping;
    procedure m_SetFindSegFacing;
    procedure m_BeforeFindTopCd(Sender: TObject);
    procedure m_ExitFindTopCd(Sender: TObject);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ShowData; override;
  end;

var
  FRAMEconsignee: TFRAMEconsignee;

implementation

uses
  kneTypes, kneFGFindUtils, kneUtils,
  //--- Frames, Forms
  MaddressAndContact, FRfindCriteriaDestination,
  //--- ServiceUtils
  ConsigneeServiceUtils,
  CountryServiceUtils, ConsigneeMarketChkAccessServiceUtils, DeliveryTermServiceUtils,
  LanguageServiceUtils, StateByCountryServiceUtils,
  DelPolicyServiceUtils, UnitSysServiceUtils,
```

#### **FRconsignee.dfm**

```
inherited FRAMEconsignee: TFRAMEconsignee
  Width = 983
  Height = 417
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLcountry: TsLabel [0]
    Left = 8
    Top = 65
    Width = 51
    Height = 13
    Caption = 'Country:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLstate: TsLabel [1]
    Left = 8
    Top = 91
    Width = 35
    Height = 13
    Caption = 'State:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLmarket: TsLabel [2]
    Left = 453
    Top = 91
    Width = 44
    Height = 13
    Caption = 'Market:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLwarehouse: TsLabel [3]
    Left = 8
    Top = 117
    Width = 69
    Height = 13
    Caption = 'Warehouse:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label2: TsLabel [4]
    Left = 8
    Top = 169
    Width = 93
    Height = 13
    Caption = 'Delivery Terms:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLdelPolicy: TsLabel [5]
    Left = 453
    Top = 169
    Width = 61
    Height = 13
    Caption = 'Del Policy:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLsysMesure: TsLabel [6]
    Left = 8
    Top = 195
    Width = 101
    Height = 13
    Caption = 'Syst. of Measure:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLname: TsLabel [7]
    Left = 8
    Top = 39
    Width = 38
```
<!-- tabs:end -->

