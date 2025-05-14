<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRfindCriteriaSalesMan`

## 1. Overview:

### Objective:
The `FRfindCriteriaSalesMan` code snippet defines a form component that allows users to filter or search for salesmen based on specific criteria, such as "Description" and "Office." It provides a user interface for entering these criteria and generates a structured query to be used in filtering or searching operations.

### Technologies Used:
- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) framework.
- **Custom Components:** Includes custom components like `TsLabel`, `TsEdit`, and `TFRAMEFindEditSOA` for UI elements and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements:**
  - `EDTdescription` (Text Input): For entering the description of the salesman.
  - `FRAMEfindOffice` (Custom Input): For selecting or entering the office.
- **Form Actions:**
  - **Initialize:** Clears the form fields and sets up the office input.
  - **GetCriteriaValues:** Generates a structured query based on the entered criteria.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input a description and select an office to filter salesmen.
- The form generates a query with the entered criteria.

### Main Components:
- **`EDTdescription`:** A text input field for the description.
- **`FRAMEfindOffice`:** A custom input field for the office.
- **`GetCriteriaValues`:** A function that constructs the filtering criteria based on user input.

### Pseudo-code for Actions and Events:
- **On Form Initialization:**
  ```
  if form initializes then
    clear description field
    clear office field
  ```
- **On GetCriteriaValues:**
  ```
  if description field is not empty then
    add description criteria to query
  if office field is not empty then
    add office criteria to query
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The `Initialize` method clears the input fields and sets up the office input.
2. **User Interaction:**
   - Users enter a description and/or select an office.
3. **Query Generation:**
   - The `GetCriteriaValues` method constructs a query based on the entered criteria.

### Data Input:
- **Description:** Text input in `EDTdescription`.
- **Office:** Text input or selection in `FRAMEfindOffice`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **GetCriteriaValues:**
  - Preconditions:
    - `FieldDescription` and `FieldOffice` must be defined.
    - At least one of the fields (`EDTdescription` or `FRAMEfindOffice`) must have a value.

### Available Filters:
- **Description:** Free text input.
- **Office:** Text input or selection.

### Error Messages:
- "Field Description must be defined" if `FieldDescription` is not set.
- "Field Office must be defined" if `FieldOffice` is not set.

### Default Field Values:
- `EDTdescription`: Empty.
- `FRAMEfindOffice`: Cleared.

### Field Validation and Conditions:
- **Description Field:**
  - Converts input to uppercase.
- **Office Field:**
  - Validates input through the custom `FRAMEfindOffice` component.

---

## 5. Main Functions:

### `Initialize`:
- Clears the input fields and sets up the office input.

### `GetCriteriaValues`:
- Constructs a query based on the entered description and office.

---

## 6. API Service Consumption:

No external API services are consumed in this code snippet.

---

## 7. Conditional Fields (Form Logic):

- The "Office" field is always visible and does not depend on other fields.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components:** Used for UI elements and functionality.
- **Custom Components:**
  - `TFRAMEFindEditSOA`: A custom component for office input.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Description (`EDTdescription`):**
   - Type: String.
   - Required: No.
   - Validation: Converts input to uppercase.
2. **Office (`FRAMEfindOffice`):**
   - Type: String.
   - Required: No.
   - Validation: Custom validation through `FRAMEfindOffice`.

### Mapping:
- `EDTdescription` → `FFieldDescription`.
- `FRAMEfindOffice` → `FFieldOffice`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [User Inputs Description/Office] --> [Generate Query] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Inputs Description/Office
Form --> GetCriteriaValues: Generate Query
GetCriteriaValues --> Form: Return Query
```

### Code Snippets:
```pascal
FRAMEfindCriteriaSalesMan.Initialize;
FRAMEfindCriteriaSalesMan.GetCriteriaValues;
```

### Screenshots:
The following HTML represents the form layout:
```html
<div style="width: 525px; height: 34px;">
  <label for="description" style="position: absolute; left: 6px; top: 14px;">Description:</label>
  <input id="description" type="text" style="position: absolute; left: 66px; top: 10px; width: 152px;" />
  
  <label for="office" style="position: absolute; left: 224px; top: 13px;">Office:</label>
  <input id="office" type="text" style="position: absolute; left: 267px; top: 8px; width: 250px;" />
</div>
```

---

## 11. Important Comments in the Code:

- **Error Handling in `GetCriteriaValues`:**
  - Ensures that `FieldDescription` and `FieldOffice` are defined before generating the query.
- **Initialization Logic:**
  - Clears fields and sets up the office input in `Initialize`.

---

## 12. Conclusion:

The `FRfindCriteriaSalesMan` component provides a simple and effective way to filter salesmen based on description and office. It ensures proper validation and query generation. However, it lacks advanced error handling and does not integrate with external APIs.

---

## 13. Short Summary:

The `FRfindCriteriaSalesMan` form allows users to filter salesmen by description and office, generating structured queries for search operations. It ensures input validation and provides a clean, user-friendly interface.#### **FRfindCriteriaSalesMan.pas**

```
unit FRfindCriteriaSalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRfindCriteria, sFrameAdapter, kneFRFindEditSOA, StdCtrls,
  sEdit, sLabel, kneConfigObjects, Buttons;

type
  TFRAMEfindCriteriaSalesMan = class(TFRAMEfindCriteria)
    LBL4: TsLabel;
    sLabel1: TsLabel;
    EDTdescription: TsEdit;
    FRAMEfindOffice: TFRAMEFindEditSOA;
  private
    FFieldDescription: string;
    FFieldOffice: string;
    { Private declarations }
    procedure m_SetFindOffice;
    procedure SetFieldDescription(const Value: string);
    procedure SetFieldOffice(const Value: string);
  protected
    { Potected declarations }
    function GetCriteriaValues: TArrayOfFieldCriteria; override;
  public
    { Public declarations }
    property FieldDescription: string read FFieldDescription write SetFieldDescription;
    property FieldOffice: string read FFieldOffice write SetFieldOffice;
    procedure Initialize; override;
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEfindCriteriaSalesMan: TFRAMEfindCriteriaSalesMan;

implementation

uses OfficeServiceUtils;

{$R *.dfm}

{ TFRAMEfindCriteriaCustomer }

constructor TFRAMEfindCriteriaSalesMan.Create(AOwner: TComponent);
begin
  inherited;

end;

function TFRAMEfindCriteriaSalesMan.GetCriteriaValues: TArrayOfFieldCriteria;
begin
  Result := nil;

  Assert(FFieldDescription <> '', 'Field Description must be defined');
  Assert(FieldOffice <> '', 'Field Office must be defined');

  // Description
  if EDTdescription.Text <> '' then
  begin
    // adicionar novo elemento
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TFieldCriteria.Create;

    Result[High(Result)].logical  := 'AND'; // Operador l�gico: {AND,OR}
    Result[High(Result)].field    := FFieldDescription; // nome do campo do dataset associado
    Result[High(Result)].operator := 'MATCHES'; // Operador a usar: {<, <=, =, >=, >, <>, LIKE}
    Result[High(Result)].value    := EDTdescription.Text; // valor
  end;

  // Office
  if FRAMEfindOffice.Text <> '' then
  begin
    // adicionar novo elemento
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := TFieldCriteria.Create;

    Result[High(Result)].logical  := 'AND'; // Operador l�gico: {AND,OR}
    Result[High(Result)].field    := FFieldOffice; // nome do campo do dataset associado
    Result[High(Result)].operator := '='; // Operador a usar: {<, <=, =, >=, >, <>, LIKE}
    Result[High(Result)].value    := FRAMEfindOffice.Text; // valor
  end;
end;

procedure TFRAMEfindCriteriaSalesMan.Initialize;
begin
  inherited;

  m_SetFindOffice;

  EDTdescription.Text := '';
  FRAMEfindOffice.Clear;
end;

procedure TFRAMEfindCriteriaSalesMan.m_SetFindOffice;
begin
  with FRAMEfindOffice do
  begin
    // objecto configurador para FindEdit
```

#### **FRfindCriteriaSalesMan.dfm**

```
inherited FRAMEfindCriteriaSalesMan: TFRAMEfindCriteriaSalesMan
  Width = 525
  Height = 34
  object LBL4: TsLabel [0]
    Left = 224
    Top = 13
    Width = 31
    Height = 13
    Caption = 'O&ffice:'
    FocusControl = FRAMEfindOffice.FE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object sLabel1: TsLabel [1]
    Left = 6
    Top = 14
    Width = 56
    Height = 13
    Caption = 'Descri&ption:'
    FocusControl = EDTdescription
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object EDTdescription: TsEdit [2]
    Left = 66
    Top = 10
    Width = 152
    Height = 21
    CharCase = ecUpperCase
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
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
  inline FRAMEfindOffice: TFRAMEFindEditSOA [3]
    Left = 267
    Top = 8
    Width = 250
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
    TabOrder = 0
    inherited PNLdesc: TPanel
      Left = 73
      Width = 177
      inherited DBEDesc: TsDBEdit
        Width = 183
      end
      inherited EDDesc: TsEdit
        Width = 177
      end
    end
    inherited PNLcode: TPanel
      Width = 73
      inherited DBE: TsDBEdit
        Width = 52
      end
      inherited FE: TsMaskEdit
        Width = 52
      end
      inherited PNLbutton: TPanel
        Left = 52
      end
    end
  end
  inherited SKFAskin: TsFrameAdapter
    Left = 456
  end
end
```
<!-- tabs:end -->

