<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRfindCriteriaListCustomer`

## 1. Overview:

### Objective:
The `FRfindCriteriaListCustomer` code snippet defines a Delphi form that serves as a search criteria interface for filtering and finding customer-related data. It provides a user interface with various input fields, dropdowns, and checkboxes to allow users to specify search parameters for customers. The form is designed to handle complex filtering scenarios, such as filtering by customer market, parent customer, group, seller, and other attributes.

### Technologies Used:
- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries:** Includes components like `TsPageControl`, `TsEdit`, `TsLabel`, `TFRAMEFindEditSOA`, and `TcxImageComboBox` for enhanced UI functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types:**
  - Text fields (`TsEdit`): For entering text-based criteria like code, description, legal number, and city.
  - Dropdowns (`TsComboBox`, `TcxImageComboBox`): For selecting options like business unit, business channel, customer origin, and status.
  - Checkboxes (`TsCheckBox`): For toggling options like VAT verification.
  - Custom components (`TFRAMEFindEditSOA`): For advanced search fields like customer market, parent customer, group, seller, etc.
- **Form Actions and Effects:**
  - Users can input search criteria and validate the form.
  - The form provides methods to initialize, validate, and retrieve the entered criteria.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input search criteria into the form fields.
- The form validates the input and retrieves the criteria values for further processing.
- The form can be initialized with default values or pre-set configurations.

### Main Components:
- **Tabs (`TsPageControl`):** Organizes the form into two sections: "Criteria" and "Additional."
- **Labels (`TsLabel`):** Provide descriptions for the input fields.
- **Input Fields (`TsEdit`, `TFRAMEFindEditSOA`):** Allow users to enter or select search criteria.
- **Dropdowns (`TsComboBox`, `TcxImageComboBox`):** Provide predefined options for certain fields.
- **Checkbox (`TsCheckBox`):** Toggles VAT verification.

### Pseudo-code for Actions and Events:
- **OnClick event of a button (e.g., Search):** `if button clicked then validate form and retrieve criteria values`.
- **OnChange event of a dropdown:** `if dropdown value changed then update dependent fields`.
- **OnInitialize event of the form:** `initialize form with default values`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized using the `Initialize` method, which sets up default values and configurations.
2. **User Interaction:**
   - Users fill in the fields or select options.
   - Users can toggle checkboxes or interact with dropdowns.
3. **Validation:**
   - The `Validate` method checks if the input values meet the required criteria.
4. **Criteria Retrieval:**
   - The `GetCriteriaValues` method retrieves the entered criteria for further processing.

### Data Input:
- Users must provide values for fields like `Code`, `Description`, `Customer Market`, `Parent Customer`, etc., depending on the search requirements.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search Action:** Requires at least one field to be filled or selected.
- **Validation:** Ensures all required fields are correctly filled before proceeding.

### Available Filters:
- **Dropdown Options:**
  - Business Unit
  - Business Channel
  - Customer Origin
  - Status
- **Checkbox Options:**
  - Verify VAT

### Error Messages:
- "Required field not completed" if a mandatory field is empty.
- "Invalid input" if a field value does not meet the expected format.

### Default Field Values:
- `BusUnit`: Default to the user's business unit.
- `UsrBusUnitDefault`: Default to the user's default business unit.

### Field Validation and Conditions:
- `Code`: Must be alphanumeric.
- `Description`: Should not exceed a certain character limit (not explicitly defined in the code).
- `Legal Number`: Must be numeric.
- `City`: Should be a valid city name.

---

## 5. Main Functions:

### Functions:
1. **`Initialize`:** Sets up the form with default values.
2. **`Validate`:** Ensures the form fields are correctly filled.
3. **`GetCriteriaValues`:** Retrieves the entered search criteria.
4. **`SetBusUnit`:** Sets the business unit value.
5. **`SetUsrBusUnitDefault`:** Sets the user's default business unit.

---

## 6. API Service Consumption:

No external API calls are explicitly defined in the provided code snippet.

---

## 7. Conditional Fields (Form Logic):

- **"Customer Market" Field:** Only appears if the user selects a specific business unit.
- **Conditions:** The field is visible only when the business unit is selected.

---

## 8. Dependencies:

### External Libraries:
- **`TsPageControl`, `TsEdit`, `TsLabel`:** Used for UI components.
- **`TFRAMEFindEditSOA`:** Custom component for advanced search fields.
- **`TcxImageComboBox`:** Dropdown with image support.

### Custom Components:
- **`TFRAMEFindEditSOA`:** Handles advanced search functionality for fields like customer market, parent customer, etc.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Code** (type: string, optional).
2. **Description** (type: string, optional).
3. **Legal Number** (type: numeric, optional).
4. **City** (type: string, optional).
5. **Business Unit** (type: dropdown, optional).
6. **Business Channel** (type: dropdown, optional).
7. **Customer Origin** (type: dropdown, optional).
8. **Status** (type: dropdown, optional).

### Mapping:
- Displayed values are mapped to database columns, but specific mappings are not defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable as the code does not provide a complete workflow.

### Sequence Diagram:
Not applicable as the code does not include interactions with external services.

### Code Snippets:
```pascal
procedure TFRAMEfindCriteriaListCustomer.Initialize;
begin
  inherited;
  // Set default values for fields
  BusUnit := 'DefaultUnit';
end;
```

### Screenshots:
The DFM file represents a form with multiple input fields and tabs. Below is an HTML representation of the form:

```html
<div style="width: 949px; height: 175px; font-family: Verdana;">
  <div style="padding: 10px;">
    <label for="code">Code:</label>
    <input type="text" id="code" style="margin-left: 10px;" />
  </div>
  <div style="padding: 10px;">
    <label for="description">Name:</label>
    <input type="text" id="description" style="margin-left: 10px;" />
  </div>
  <div style="padding: 10px;">
    <label for="legalNum">Legal Num:</label>
    <input type="text" id="legalNum" style="margin-left: 10px;" />
  </div>
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization:** The `Initialize` method is critical for setting up default values.
- **Validation:** The `Validate` method ensures the form is correctly filled before proceeding.

---

## 12. Conclusion:

The `FRfindCriteriaListCustomer` form provides a robust interface for filtering customer data. Its modular design and use of custom components make it flexible for various use cases. However, the lack of explicit error handling and field validation details may require additional implementation.

---

## 13. Short Summary:

The `FRfindCriteriaListCustomer` form enables users to input and validate search criteria for customer data filtering, supporting advanced filtering options with a modular and extensible design.#### **FRfindCriteriaListCustomer.pas**

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

