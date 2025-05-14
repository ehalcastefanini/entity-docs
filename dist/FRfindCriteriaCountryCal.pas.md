<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRfindCriteriaCountryCal`

## 1. Overview:

### Objective and Problem Solved:
The `FRfindCriteriaCountryCal` code snippet defines a form component that allows users to filter data based on specific criteria: an event date and a country code. This component is part of a larger system that likely involves querying or filtering datasets based on user-defined criteria. The form provides a user-friendly interface for selecting a date, a country, and a year, and it generates the corresponding filtering criteria.

### Technologies Used:
- **Delphi (Object Pascal):** The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Third-party Libraries:** Includes components like `TsLabel`, `TsCheckBox`, `TcxDateEdit`, and `TsComboBox` for enhanced UI functionality.
- **Custom Components:** `TFRAMEFindEditSOA` is a custom component used for country selection.

### Form Type:
This is a **form** with the following elements:
- **Form Elements:**
  - `CHKeventDate` (Checkbox): Enables or disables the event date filter.
  - `DTEeventDate` (Date Picker): Allows the user to select an event date.
  - `FRAMEfindCountry` (Custom Component): Allows the user to select a country.
  - `CBOyear` (ComboBox): Allows the user to select a year.
- **Form Actions:**
  - Clicking the checkbox enables or disables the date picker.
  - Selecting a country or year updates the filtering criteria.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can enable the event date filter by checking the `CHKeventDate` checkbox.
- Users can select a specific event date using the `DTEeventDate` date picker.
- Users can select a country using the `FRAMEfindCountry` component.
- Users can select a year from the `CBOyear` dropdown.

### Main Components:
1. **`CHKeventDate` Checkbox:** Toggles the availability of the event date filter.
2. **`DTEeventDate` Date Picker:** Allows users to select a specific date.
3. **`FRAMEfindCountry` Component:** Provides a UI for selecting a country.
4. **`CBOyear` ComboBox:** Displays a list of years for selection.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHKeventDate`:
  ```pseudo
  if CHKeventDate is checked then
    enable DTEeventDate
  else
    disable DTEeventDate
  ```
- `OnChange` event of `FRAMEfindCountry`:
  ```pseudo
  if country is selected then
    update criteria with selected country
  ```
- `OnChange` event of `CBOyear`:
  ```pseudo
  if year is selected then
    update criteria with selected year
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The form is initialized with the current year and a range of years in the `CBOyear` dropdown.
   - The `DTEeventDate` is set to the current date.
   - The `CHKeventDate` checkbox is unchecked by default, disabling the date picker.

2. **User Interactions:**
   - Users can check the `CHKeventDate` checkbox to enable the date picker.
   - Users can select a country and year to define filtering criteria.

### Functions:
- **`Create` (File: `FRfindCriteriaCountryCal`):**
  - Initializes the form, populates the year dropdown, and sets default values.
- **`GetCriteriaValues` (File: `FRfindCriteriaCountryCal`):**
  - Generates an array of filtering criteria based on user input.

### Required User Data:
- Event date (optional, enabled by `CHKeventDate`).
- Country code (optional).
- Year (optional).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Enable Event Date Filter:**
  - Preconditions: `CHKeventDate` must be checked.
  - Action: Enables the `DTEeventDate` date picker.
- **Generate Criteria:**
  - Preconditions: At least one filter (event date, country, or year) must be selected.
  - Action: Generates filtering criteria for querying datasets.

### Available Filters:
- Event Date: Enabled by checking `CHKeventDate`.
- Country: Selected via `FRAMEfindCountry`.
- Year: Selected via `CBOyear`.

### Error Messages:
- "Invalid date" if the selected date is not valid (handled by `TcxDateEdit`).
- "Country not selected" if no country is selected (not explicitly handled in the code).

### Default Field Values:
- `CHKeventDate`: Unchecked.
- `DTEeventDate`: Current date.
- `CBOyear`: Current year.

### Field Validation and Conditions:
- `DTEeventDate`: Must be a valid date.
- `FRAMEfindCountry`: Must contain a valid country code.
- `CBOyear`: Must be a valid year within the range.

---

## 5. Main Functions:

1. **`Create`:**
   - Initializes the form, populates the year dropdown, and sets default values.
2. **`GetCriteriaValues`:**
   - Generates an array of filtering criteria based on user input.
3. **`CHKeventDateClick`:**
   - Toggles the availability of the `DTEeventDate` date picker.

---

## 6. API Service Consumption:

No external API calls are made in this code snippet.

---

## 7. Conditional Fields (Form Logic):

- **`DTEeventDate`:**
  - Visible and enabled only when `CHKeventDate` is checked.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components:** Used for UI elements.
- **Third-party Components:**
  - `TsLabel`, `TsCheckBox`, `TcxDateEdit`, `TsComboBox`: Enhanced UI components.

### Custom Components:
- **`TFRAMEFindEditSOA`:** Custom component for country selection.

---

## 9. Fields and Validations Listing:

1. **Event Date (`DTEeventDate`):**
   - Type: Date.
   - Required: No.
   - Default: Current date.
2. **Country (`FRAMEfindCountry`):**
   - Type: String.
   - Required: No.
   - Default: None.
3. **Year (`CBOyear`):**
   - Type: Integer.
   - Required: No.
   - Default: Current year.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [User Input] --> [Generate Criteria] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Selects filters
Form --> Backend: Sends criteria
Backend --> Form: Returns filtered data
```

### Code Snippets:
```pascal
CHKeventDate.OnClick := CHKeventDateClick;
DTEeventDate.Date := Today;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 509px;">
  <label for="country">Country:</label>
  <input type="text" id="country" style="width: 381px;">
  <label for="year">Year:</label>
  <select id="year" style="width: 65px;">
    <option>2023</option>
    <option>2024</option>
  </select>
  <input type="checkbox" id="eventDate"> Event Date:
  <input type="date" id="datePicker" disabled>
</div>
```

---

## 11. Important Comments in the Code:

- **Year Dropdown Initialization:**
  ```pascal
  for lv_i := lv_Year - 5 to lv_Year + 5 do
  begin
    CBOyear.Items.Add(IntToStr(lv_i));
  end;
  ```
- **Criteria Generation:**
  ```pascal
  if CHKeventDate.Checked then
  begin
    // Add event date criteria
  end;
  ```

---

## 12. Conclusion:

The `FRfindCriteriaCountryCal` component provides a flexible and user-friendly interface for filtering data based on event date, country, and year. While it is well-structured, error handling for invalid inputs could be improved. Its modular design makes it easy to integrate into larger systems.

---

## 13. Short Summary:

The `FRfindCriteriaCountryCal` form allows users to filter data by event date, country, and year. It initializes default values, dynamically generates criteria, and provides a user-friendly interface for data filtering.#### **FRfindCriteriaCountryCal.pas**

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

