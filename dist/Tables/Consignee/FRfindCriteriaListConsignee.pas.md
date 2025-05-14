<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRfindCriteriaListConsignee`

## 1. Overview:

### Objective and Problem Solved:
The `FRfindCriteriaListConsignee` code snippet defines a Delphi form (`TFRAMEfindCriteriaListConsignee`) that provides a user interface for filtering and searching consignee-related data based on various criteria such as code, description, country, market, postal code, destination, mill, and status. This form is part of a larger system that likely deals with managing consignee information.

The main objective of this form is to allow users to input search criteria and retrieve filtered results based on the provided inputs. It simplifies the process of narrowing down consignee data by providing a structured and user-friendly interface.

### Technologies Used:
- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Custom Components:** Includes custom components like `TsLabel`, `TsEdit`, `TFRAMEFindEditSOA`, and `TcxImageComboBox`.
- **Service Utilities:** Utilizes external service utilities such as `ConsigneeMarketServiceUtils`, `CountryServiceUtils`, and `MillServiceUtils`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Labels (`TsLabel`): Display text for form fields.
  - Text Inputs (`TsEdit`): For entering text-based criteria (e.g., code, description, postal code).
  - Dropdowns (`TcxImageComboBox`): For selecting predefined options (e.g., status).
  - Custom Search Fields (`TFRAMEFindEditSOA`): For selecting country, market, destination, and mill.
- **Form Actions and Effects:**
  - Users can input search criteria and trigger a search operation.
  - The form validates the input fields and ensures required fields are filled before proceeding.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input search criteria in the provided fields.
- The form validates the input and retrieves filtered results based on the criteria.

### Main Components:
1. **Labels (`TsLabel`):** Provide descriptions for each input field.
2. **Text Inputs (`TsEdit`):** Allow users to enter text-based search criteria.
3. **Dropdown (`TcxImageComboBox`):** Allows users to select a status from predefined options.
4. **Custom Search Fields (`TFRAMEFindEditSOA`):** Provide advanced search functionality for country, market, destination, and mill.

### Pseudo-code for Actions and Events:
- **On Form Initialization:**
  - `initialize form components`
  - `set default values for dropdowns`
- **On Search Button Click:**
  - `if all required fields are filled then execute search`
- **On Field Value Change:**
  - `if field value changed then validate field`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized using the `Create` constructor.
   - Default values are set for dropdowns and other components.
2. **User Interaction:**
   - Users fill in the search criteria fields.
   - The form validates the input and ensures required fields are filled.
3. **Search Execution:**
   - The `GetCriteriaValues` function retrieves the input values and prepares them for further processing.

### Data Required:
- Code
- Description
- Country
- Market
- Postal Code
- Destination
- Mill
- Status

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search Action:**
  - Preconditions: All required fields (e.g., code, description, country) must be filled.
  - Action: Retrieves filtered results based on the input criteria.

### Available Filters:
- Country
- Market
- Destination
- Mill
- Status

### Error Messages:
- "Field Code must be defined" if the code field is empty.
- "Field Description must be defined" if the description field is empty.
- "Field Country must be defined" if the country field is empty.

### Default Field Values:
- Status: Defaults to the first item in the dropdown.

### Field Validation and Conditions:
- Code: Must be uppercase.
- Description: No specific validation defined.
- Country, Market, Destination, Mill: Must be selected using the custom search fields.

---

## 5. Main Functions:

1. **`Create`:** Initializes the form and sets default values.
2. **`GetCriteriaValues`:** Retrieves the input values and validates them.
3. **`m_SetFindCountry`, `m_SetFindConsMarket`, `m_SetFindDestination`, `m_SetFindMill`:** Configure the custom search fields.

---

## 6. API Service Consumption:

- **Service Name:** ConsigneeMarketServiceUtils, CountryServiceUtils, MillServiceUtils
- **Purpose:** These services are used to populate and manage the custom search fields for market, country, and mill.

---

## 7. Conditional Fields (Form Logic):

- The "Status" dropdown is only enabled if there are items in its list.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components:** Used for UI elements.
- **Custom Components:** `TsLabel`, `TsEdit`, `TFRAMEFindEditSOA`, `TcxImageComboBox`.

### Custom Components:
- **`TFRAMEFindEditSOA`:** A custom component for advanced search functionality.

---

## 9. Fields and Validations Listing:

1. **Code (EDTcode):**
   - Type: String
   - Required: No
   - Validation: Must be uppercase.
2. **Description (EDTdescription):**
   - Type: String
   - Required: No
   - Validation: Must be uppercase.
3. **Country (FRAMEfindCountry):**
   - Type: Custom Search Field
   - Required: No
4. **Market (FRAMEfindConsMarket):**
   - Type: Custom Search Field
   - Required: No
5. **Postal Code (EDTpostal):**
   - Type: String
   - Required: No
   - Validation: Must be uppercase.
6. **Destination (FRAMEfindDestination):**
   - Type: Custom Search Field
   - Required: No
7. **Mill (FRAMEfindMill):**
   - Type: Custom Search Field
   - Required: No
8. **Status (CBOstat):**
   - Type: Dropdown
   - Required: No
   - Default: First item in the list.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a complete workflow.)

### Sequence Diagram:
(Not applicable as the code does not include interactions with external systems.)

### Code Snippets:
```pascal
procedure TFRAMEfindCriteriaListConsignee.Initialize;
begin
  inherited;
  m_SetFindCountry;
  m_SetFindConsMarket;
  m_SetFindDestination;
  m_SetFindMill;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 749px; height: 190px; font-family: Verdana;">
  <label for="code">Code:</label>
  <input id="code" type="text" style="text-transform: uppercase;" />
  <label for="description">Name:</label>
  <input id="description" type="text" />
  <label for="country">Country:</label>
  <input id="country" type="text" />
  <label for="market">Market:</label>
  <input id="market" type="text" />
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
    <option>Pending</option>
    <option>ALL</option>
  </select>
</div>
```

---

## 11. Important Comments in the Code:

- `//#24509 (cmosilva 12-08-2021)`: Indicates a specific change or feature addition related to the status dropdown.

---

## 12. Conclusion:

The `FRfindCriteriaListConsignee` form provides a structured interface for filtering consignee data. It is well-designed for extensibility and integrates custom components for advanced search functionality. However, the code lacks detailed error handling and validation for some fields.

---

## 13. Short Summary:

The `FRfindCriteriaListConsignee` form allows users to filter consignee data using various criteria. It integrates custom search fields and dropdowns for enhanced functionality, ensuring a user-friendly experience.#### **FRfindCriteriaListConsignee.pas**

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

