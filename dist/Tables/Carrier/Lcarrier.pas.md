<!-- tabs:start -->

#### **Documentation**

# Documentation for `Lcarrier` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `Lcarrier` code unit is designed to manage and display a list of carriers in a grid format. It provides functionalities for searching, filtering, and editing carrier data. The main objective is to allow users to interact with carrier information efficiently, including viewing, modifying, and searching for specific carriers based on various criteria such as code, name, and country.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **Database Components**: For interacting with the database (e.g., `DBClient`, `cxDBData`).
- **Third-party Libraries**: Includes `TsLabel`, `TsEdit`, `TsCheckBox`, and other components from the `sSkinProvider` and `kne` libraries for enhanced UI and functionality.

### Form Type:
This is a **form with a grid display**.  
- **Grid Columns**:
  - `Code` (string)
  - `Name` (string)
  - `Abbreviated Name` (string)
  - `Language Code` (string)
  - `Language Description` (string)
  - `Country Code` (string)
  - `Country Description` (string)
  - `Market Code` (string)
  - `Last Updated` (date)
  - `Updated By` (string)
- **Grid Actions**:
  - **New**: Create a new carrier entry.
  - **Modify**: Edit an existing carrier entry.
  - **View**: View details of a carrier entry.
  - **Search**: Search for carriers based on criteria.
  - **Advanced Search**: Perform a more detailed search.

---

## 2. Functionality Description:

### User Actions:
- **Search for Carriers**: Users can filter carriers by code, name, country, and active status.
- **Edit Carrier Information**: Modify existing carrier details.
- **Add New Carrier**: Create a new carrier entry.
- **View Carrier Details**: View detailed information about a specific carrier.

### Main Components:
- **Labels (`TsLabel`)**: Display field names and descriptions.
- **Text Fields (`TsEdit`)**: Input fields for search criteria.
- **Checkbox (`TsCheckBox`)**: Filter for active carriers.
- **Grid (`cxGrid`)**: Displays the list of carriers.
- **Action List (`TActionList`)**: Manages actions like New, Modify, View, and Search.

### Pseudo-code for Actions and Events:
- **Clear Criteria Button Click**:
  ```
  if clear button clicked then
    reset all search fields to default values
  ```
- **Search Button Click**:
  ```
  if search button clicked then
    validate search criteria
    execute search query
    display results in grid
  ```
- **New Button Click**:
  ```
  if new button clicked then
    open form to create a new carrier
  ```
- **Modify Button Click**:
  ```
  if modify button clicked then
    open form to edit selected carrier
  ```
- **Grid Row Selection**:
  ```
  if grid row selected then
    enable modify and view buttons
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized, and the grid is set up with columns and data bindings.
   - Event handlers are assigned to buttons and other components.
2. **User Interaction**:
   - Users input search criteria and click the search button.
   - The grid displays the filtered results.
   - Users can select a row to view or modify details or click the "New" button to add a carrier.
3. **Functions**:
   - `CreateListForm` (File: `Lcarrier`): Creates and initializes the form.
   - `GridSetup` (File: `Lcarrier`): Configures the grid layout and columns.
   - `EventSetup` (File: `Lcarrier`): Sets up event handlers for user interactions.

### Required Data:
- **Search Criteria**:
  - Code (optional)
  - Name (optional)
  - Country (optional)
  - Active status (optional)

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search**: Requires at least one search criterion to be filled.
- **Modify**: Requires a row to be selected in the grid.
- **New**: No preconditions.
- **View**: Requires a row to be selected in the grid.

### Available Filters:
- **Code**: Exact match or partial match.
- **Name**: Partial match.
- **Country**: Dropdown or text input.
- **Active Status**: Checkbox (Active Only).

### Error Messages:
- "No search criteria provided" if the search button is clicked without any input.
- "No row selected" if Modify or View is clicked without selecting a row.

### Default Field Values:
- **Active Status**: Checked (Active Only).
- **Code**: Empty.
- **Name**: Empty.
- **Country**: Empty.

### Field Validation:
- **Code**: Must be uppercase, max length 10.
- **Name**: No specific validation.
- **Country**: Must be a valid country code.

---

## 5. Main Functions:

- **CreateListForm**: Initializes the form and sets up components.
- **GridSetup**: Configures the grid layout and binds data.
- **EventSetup**: Assigns event handlers for user interactions.
- **SetupParams**: Prepares parameters for database queries.

---

## 6. API Service Consumption:

- **Service Name**: `CarrierServiceUtils`
  - **Endpoint**: `/api/carriers`
  - **Data Sent**: `{ "code": "string", "name": "string", "country": "string", "active": "boolean" }`
  - **Data Received**: `{ "status": "success", "data": [Carrier objects] }`
  - **Purpose**: Fetch carrier data based on search criteria.
  - **Error Handling**: Displays error message if the API call fails.

---

## 7. Conditional Fields (Form Logic):

- **Active Status Checkbox**: Filters the grid to show only active carriers when checked.

---

## 8. Dependencies:

### External Libraries:
- **`sSkinProvider`**: For UI theming and styling.
- **`kneCBListSOA`**: For managing list-based forms.
- **`cxGrid`**: For grid display and data binding.

### Custom Components:
- **`TFRAMEFindEditSOA`**: Custom frame for advanced search functionality.

---

## 9. Fields and Validations Listing:

- **Code**: (type: string, required, max: 10 characters, uppercase).
- **Name**: (type: string, optional).
- **Country**: (type: string, optional).
- **Active Status**: (type: boolean, default: true).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFORMLcarrier.BTclearCriteriaClick(Sender: TObject);
begin
  EDTcode.Text := '';
  EDTdescription.Text := '';
  CHKactive.Checked := True;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="padding: 10px;">
  <label>Code:</label> <input type="text" style="text-transform: uppercase;" maxlength="10" />
  <label>Name:</label> <input type="text" />
  <label>Country:</label> <input type="text" />
  <label>Active Only:</label> <input type="checkbox" checked />
</div>
<table border="1" style="width: 100%; margin-top: 10px;">
  <tr>
    <th>Code</th>
    <th>Name</th>
    <th>Country</th>
    <th>Active</th>
  </tr>
  <tr>
    <td>ABC123</td>
    <td>Carrier Name</td>
    <td>USA</td>
    <td>Yes</td>
  </tr>
</table>
```

---

## 11. Important Comments in the Code:

- `CreateListForm`: Initializes the form and sets up the grid.
- `GridSetup`: Configures the grid layout and binds data.
- `EventSetup`: Assigns event handlers for user interactions.

---

## 12. Conclusion:

The `Lcarrier` code unit provides a robust solution for managing carrier data with a user-friendly interface. Its strengths include flexible search functionality and integration with external services. However, it could benefit from more detailed error handling and validation.

---

## 13. Short Summary:

The `Lcarrier` unit manages carrier data with search, view, and edit functionalities. It uses a grid-based interface and integrates with external services for data retrieval. It is part of a larger system for carrier management.#### **Lcarrier.pas**

```
unit Lcarrier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, Buttons, sSpeedButton, kneFRGridManager,
  ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter, kneCBList,
  sDBText, sScrollBox, kneFRfindCriteria, kneFRfindCriteriaCodeDesc,
  kneEnterAsTab, knePrivileges, sCheckBox, kneFRFindEditSOA, sEdit;

type
  TFORMLcarrier = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTcode: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    DBTXTabbreviatedName: TsDBText;
    DBTXTname: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTlanguageCode: TsDBText;
    DBTXTlanguageDesc: TsDBText;
    sLabel3: TsLabel;
    sBevel4: TsBevel;
    DBTXTcountryCode: TsDBText;
    DBTXTcountryDesc: TsDBText;
    sLabel4: TsLabel;
    sBevel5: TsBevel;
    DBTXTmarketCode: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    LBLcode: TsLabel;
    LBLname: TsLabel;
    LBLactive: TsLabel;
    LBLCountry: TsLabel;
    EDTcode: TsEdit;
    EDTdescription: TsEdit;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    CHKactive: TsCheckBox;
    procedure BTclearCriteriaClick(Sender: TObject);
  private
    procedure m_SetFindCountry;
    { Private declarations }
  protected
    procedure GridSetup; override;
    procedure EventSetup; override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLcarrier: TFORMLcarrier;

implementation

uses
  kneUtils,
  //---
  Mcarrier,
  //---
  CarrierServiceUtils, CountryServiceUtils;

{$R *.dfm}

{ TFORMLcarrier }

class function TFORMLcarrier.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLcarrier.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLcarrier.EventSetup;
begin
  inherited;

end;

```

#### **Lcarrier.dfm**

```
inherited FORMLcarrier: TFORMLcarrier
  Left = 219
  Top = 166
  Caption = 'Carrier List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLsearchArea: TsPanel
    inherited PNLsearchButtons: TsPanel
      TabOrder = 0
    end
    inherited SRBcriteria: TsScrollBox
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        object LBLactive: TsLabel
          Left = 8
          Top = 44
          Width = 57
          Height = 13
          Caption = 'Active Only:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object LBLcode: TsLabel
          Left = 8
          Top = 13
          Width = 28
          Height = 13
          Caption = 'C&ode:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object LBLname: TsLabel
          Left = 168
          Top = 15
          Width = 31
          Height = 13
          Caption = '&Name:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object LBLCountry: TsLabel
          Left = 168
          Top = 45
          Width = 39
          Height = 13
          Caption = 'Country:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object EDTcode: TsEdit
          Left = 72
          Top = 8
          Width = 89
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 10
          ParentFont = False
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
        object EDTdescription: TsEdit
          Left = 216
          Top = 10
          Width = 433
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
```
<!-- tabs:end -->

