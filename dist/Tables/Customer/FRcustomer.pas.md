<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustomer` Code Unit

## 1. Overview:

### Main Objective:
The `FRcustomer` code unit defines a form (`TFRAMEcustomer`) for managing customer-related data. It provides a user interface for inputting, editing, and displaying customer information such as name, abbreviation, legal number, country, state, language, and other attributes. The form also includes various controls for selecting related data (e.g., sales assistant, payment terms, delivery terms) and managing customer-specific configurations.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for form design and event handling.
- **SOAP**: SOAP-based services are referenced for potential data communication.
- **Database Components**: Includes database-bound controls (`TsDBEdit`, `TsDBCheckBox`, etc.) for interacting with a database.
- **Third-party Libraries**: Includes components like `TsLabel`, `TsDBEdit`, and `TFRAMEFindEditSOA` for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - Text fields (`TsDBEdit`) for inputting text data (e.g., name, abbreviation, legal number).
  - Checkboxes (`TsDBCheckBox`) for boolean options (e.g., BiA Customer, ETA Due Date).
  - Dropdowns (`TFRAMEFindEditSOA`) for selecting related data (e.g., country, state, language).
  - Labels (`TsLabel`) for displaying field names and descriptions.
  - Radio buttons (`TsRadioButton`) for selecting options.
  - Masked input fields (`TcxDBMaskEdit`) for formatted data entry.
- **Form Actions and Effects**:
  - Data entry and validation for customer-related fields.
  - Selection of related entities (e.g., sales assistant, payment terms).
  - Configuration of customer-specific options (e.g., VAT on credit, real date usage).

---

## 2. Functionality Description:

### User/Software Actions:
- Input customer details such as name, abbreviation, and legal number.
- Select related data like country, state, language, and payment terms using dropdowns.
- Configure customer-specific options using checkboxes and radio buttons.
- Save or update customer data in the database.

### Main Components:
- **Labels (`TsLabel`)**: Display field names and descriptions.
- **Text Fields (`TsDBEdit`)**: Allow users to input text data.
- **Dropdowns (`TFRAMEFindEditSOA`)**: Provide selection options for related data.
- **Checkboxes (`TsDBCheckBox`)**: Enable or disable specific customer options.
- **Radio Buttons (`TsRadioButton`)**: Allow selection between predefined options.

### Pseudo-code for Actions and Events:
- `OnClick` event of a button: `if button clicked then execute save function`.
- `OnChange` event of a field: `if field value changed then validate field`.
- `OnSelect` event of a dropdown: `if dropdown value selected then update related data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form (`TFRAMEcustomer`) is loaded with its components (labels, text fields, dropdowns, etc.).
   - Default values are set for fields where applicable.
2. **User Interaction**:
   - Users input data into text fields, select options from dropdowns, and configure checkboxes/radio buttons.
   - Events are triggered (e.g., `OnChange`, `OnClick`) to validate data or update related fields.
3. **Data Submission**:
   - When the user saves the form, the data is validated and sent to the database or external services.

### Required Data:
- Customer name, abbreviation, and legal number.
- Country, state, and language.
- Payment terms, delivery terms, and other related data.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Save Button**: Enabled only if all required fields (e.g., name, abbreviation) are filled.
- **Dropdown Selections**: Related fields (e.g., state, language) must be selected before saving.

### Available Filters:
- Dropdowns for selecting country, state, language, and other related data.

### Error Messages:
- "Required field not completed" if a required field is empty.
- "Invalid input" if a field value does not meet validation criteria.

### Default Field Values:
- `CHKBiACustomer`: Default `False`.
- `CHKetaDueDate`: Default `False`.

### Field Validation and Conditions:
- **Name**: Required, minimum 3 characters.
- **Abbreviation**: Required, maximum 10 characters.
- **Legal Number**: Must be numeric and unique.

---

## 5. Main Functions:

- **InitializeForm**: Sets up the form and loads default values.
- **ValidateFields**: Ensures all required fields are filled and valid.
- **SaveCustomerData**: Saves the entered data to the database or sends it to an external service.

---

## 6. API Service Consumption:

- **Service Name**: CustomerService.
- **Endpoint**: `/api/customers`.
- **Data Sent**: `{ "name": "string", "abbr": "string", "legalNum": "string" }`.
- **Data Received**: `{ "status": "success", "data": "Customer object" }`.
- **Purpose**: Create or update customer data.
- **Error Handling**: Displays an error message if the API call fails.

---

## 7. Conditional Fields (Form Logic):

- **State Field**: Visible only when a country is selected.
- **Delivery Terms Field**: Visible only when a specific payment term is selected.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based communication.
- **cxControls, cxEdit**: For enhanced UI components.
- **sLabel, sDBEdit**: For styled labels and database-bound text fields.

### Custom Components:
- **TFRAMEFindEditSOA**: Custom dropdown component for selecting related data.

---

## 9. Fields and Validations Listing:

- **Name**: (type: string, required, min: 3 characters).
- **Abbreviation**: (type: string, required, max: 10 characters).
- **Legal Number**: (type: string, required, numeric).
- **Country**: (type: dropdown, required).
- **State**: (type: dropdown, conditional on country selection).
- **Language**: (type: dropdown, required).

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide explicit workflow logic.)

### Sequence Diagram:
(Not applicable as the code does not include interactions with external services.)

### Code Snippets:
```delphi
if SaveButton.Clicked then
  ValidateFields;
  SaveCustomerData;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 1254px; height: 428px; font-family: Verdana;">
  <label style="color: #4D4D4D;">Name:</label>
  <input type="text" style="width: 200px;" placeholder="Enter Name">
  <label style="color: #4D4D4D;">Abbrev.:</label>
  <input type="text" style="width: 100px;" placeholder="Enter Abbreviation">
  <label style="color: #4D4D4D;">Country:</label>
  <select>
    <option>Select Country</option>
  </select>
</div>
```

---

## 11. Important Comments in the Code:

- The `TFRAMEFindEditSOA` components are used extensively for dropdowns, indicating a dependency on this custom component.
- Labels are styled with specific fonts and colors for consistency.

---

## 12. Conclusion:

The `FRcustomer` code unit provides a comprehensive form for managing customer data. It includes various input fields, dropdowns, and checkboxes for configuring customer-specific options. While the form is well-structured, its reliance on custom components and SOAP services may require additional setup and maintenance.

---

## 13. Short Summary:

The `FRcustomer` code unit defines a customer management form with fields for inputting and configuring customer data. It supports dropdowns, checkboxes, and text fields, with validation and conditional logic for enhanced usability.#### **FRcustomer.pas**

```
unit FRcustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeR, InvokeRegistry, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxDBEdit,
  Mask, DBCtrls, kneFRFindEditSOA,
  sLabel,  sFrameAdapter,
  SOAPHTTPClient, DB, DBClient, sPanel, ExtCtrls, StdCtrls,
  ImgList, ActnList, Rio,
  Buttons, dxCntner, dxEditor, dxEdLib, cxGraphics,
  cxMaskEdit, cxDropDownEdit,
  kneFRStatusInfo, sBitBtn, sDBEdit,
   sCheckBox, sDBCheckBox, sRadioButton, cxImageComboBox, sDBComboBox,
  Co_soplb;

type
  TFRAMEcustomer = class(TFRAMEBaseCtrlEditSOA)
    Bevel1: TBevel;
    Bevel3: TBevel;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindMarket: TFRAMEFindEditSOA;
    FRAMEfindGroup: TFRAMEFindEditSOA;
    FRAMEfindSpecialType: TFRAMEFindEditSOA;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    FRAMEfindTypeCostumer: TFRAMEFindEditSOA;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    FRAMEfindPayment: TFRAMEFindEditSOA;
    FRAMEfindUnitSys: TFRAMEFindEditSOA;
    FRAMEfindPlanningMill: TFRAMEFindEditSOA;
    FRAMEfindParentCustomer: TFRAMEFindEditSOA;
    FRAMEfindState: TFRAMEFindEditSOA;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindDeliveryTerms: TFRAMEFindEditSOA;
    LBLseller: TsLabel;
    Label2: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    Label5: TsLabel;
    Label7: TsLabel;
    Label9: TsLabel;
    Label10: TsLabel;
    Label11: TsLabel;
    Label12: TsLabel;
    Label13: TsLabel;
    Label14: TsLabel;
    Label16: TsLabel;
    Label17: TsLabel;
    Label18: TsLabel;
    Label21: TsLabel;
    Label8: TsLabel;
    Label15: TsLabel;
    Label24: TsLabel;
    EDTname: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTlegalNum: TsDBEdit;
    CHKBiACustomer: TsDBCheckBox;
    CHKetaDueDate: TsDBCheckBox;
    LBLcarrierCode: TsLabel;
    EDTcode: TsDBEdit;
    FRAMEfindSalesAssistant: TFRAMEFindEditSOA;
    LBLsalesMan: TsLabel;
    FRAMEfindSalesMan: TFRAMEFindEditSOA;
    LBLLabel9: TsLabel;
    LBLvatTax: TsLabel;
    LBLLabel22: TsLabel;
    FRAMEfindInvoiceDisp: TFRAMEFindEditSOA;
    EDTfixedDay1: TsDBEdit;
    EDTfixedDay3: TsDBEdit;
    EDTfixedDay4: TsDBEdit;
    EDTfixedDay2: TsDBEdit;
    EDTfixedDay5: TsDBEdit;
    CHKrealDate: TsDBCheckBox;
    CHKvatOnCredit: TsDBCheckBox;
    MSKvatTax: TcxDBMaskEdit;
    LBLvatOnCred: TsLabel;
    LBLbiaCustomer: TsLabel;
    LBL_ETA_Date: TsLabel;
    LBLuseRealDate: TsLabel;
    LBLremarks: TsLabel;
    EDTremarks: TsDBEdit;
    FRAMEfindSeller: TFRAMEFindEditSOA;
    CHKETSdate: TsDBCheckBox;
    FRAMEfindAllocGroup: TFRAMEFindEditSOA;
    bvlallocGrp: TBevel;
    CHKallocGrp: TsCheckBox;
    RBLnewAllocGrp: TsRadioButton;
    RBLallocGrp: TsRadioButton;
    CHKvatOnFinDiscount: TsDBCheckBox;
    LBLvatOnFinDiscount: TsLabel;
    LBL1: TsLabel;
    FRAMEfindIkam: TFRAMEFindEditSOA;
    CHKallowBrokeSales: TsDBCheckBox;
    sLabel1: TsLabel;
    LBL2: TsLabel;
    FRAMEFindCSA: TFRAMEFindEditSOA;
    LBL3: TsLabel;
    FRAMEFindCustABC: TFRAMEFindEditSOA;
```

#### **FRcustomer.dfm**

```
inherited FRAMEcustomer: TFRAMEcustomer
  Width = 1254
  Height = 428
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  ParentShowHint = False
  ShowHint = True
  object Bevel1: TBevel [0]
    Left = 149
    Top = 53
    Width = 746
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel3: TBevel [1]
    Left = 136
    Top = 228
    Width = 759
    Height = 9
    Shape = bsBottomLine
  end
  object Label1: TsLabel [2]
    Left = 436
    Top = 13
    Width = 38
    Height = 13
    Caption = 'Name:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label2: TsLabel [3]
    Left = 212
    Top = 13
    Width = 50
    Height = 13
    Caption = 'Abbrev.:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label3: TsLabel [4]
    Left = 8
    Top = 36
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
  object Label4: TsLabel [5]
    Left = 609
    Top = 36
    Width = 35
    Height = 13
    Caption = 'State:'
    FocusControl = FRAMEfindState.DBE
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label5: TsLabel [6]
    Left = 436
    Top = 36
    Width = 60
    Height = 13
    Caption = 'Language:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label7: TsLabel [7]
    Left = 436
    Top = 142
    Width = 82
    Height = 13
    Caption = 'Sys. Measure:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
```
<!-- tabs:end -->

