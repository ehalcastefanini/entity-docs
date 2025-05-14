<!-- tabs:start -->

#### **Documentation**

# Documentation for `LsalesMan` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `LsalesMan` code unit is designed to manage and display a list of sales managers in a grid format. It provides functionalities for searching, filtering, and interacting with sales manager data. The main objective is to allow users to view, filter, and manage sales manager records efficiently.

### High-Level Functionality:
- Displays a grid of sales manager data with customizable columns.
- Provides search and filter options, including filtering by name, office, and active status.
- Includes actions for creating, modifying, and viewing sales manager records.
- Integrates with external services for data retrieval and management.

### Technologies Used:
- Delphi (Object Pascal) for application development.
- Components from third-party libraries such as `cxGrid`, `TsEdit`, `TsCheckBox`, and `kneCBListSOA`.
- External services for data handling (`SalesManServiceUtils` and `OfficeServiceUtils`).

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. `stat` - Status (Custom Editor: `cxEDTstatus`).
2. `salesman` - Salesman ID (String).
3. `name` - Salesman Name (String).
4. `login` - Login (String).
5. `officeCode` - Office Code (String).
6. `officeDesc` - Office Description (String).
7. `email` - Email Address (String).
8. `lastUpd` - Last Updated (Date/Time).
9. `updBy` - Updated By (String).

#### Grid Actions and Their Effects:
- **New**: Opens a form to create a new sales manager record.
- **Modify**: Opens a form to edit the selected sales manager record.
- **View**: Opens a form to view details of the selected sales manager record.
- **Search Area**: Allows users to search for sales managers based on specific criteria.
- **Advanced Search**: Provides additional search options for more complex queries.

---

## 2. Functionality Description:

### User/Software Actions:
- **Search**: Users can filter the grid by name, office, and active status.
- **Create**: Add a new sales manager record.
- **Modify**: Edit an existing sales manager record.
- **View**: View details of a selected sales manager record.

### Main Components:
1. **Grid**: Displays the list of sales managers.
2. **Search Area**: Contains fields for filtering the grid.
3. **Actions**: Buttons and actions for creating, modifying, and viewing records.

### Pseudo-Code for Actions and Events:
- **OnClick** event of "New" button: `if button clicked then open create form`.
- **OnClick** event of "Modify" button: `if button clicked and record selected then open modify form`.
- **OnClick** event of "View" button: `if button clicked and record selected then open view form`.
- **OnChange** event of "Active Only" checkbox: `if checkbox value changed then refresh grid with active filter`.
- **OnChange** event of "Name" field: `if field value changed then filter grid by name`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `CreateListForm`.
   - The grid is set up with `GridSetup`.
   - Events are configured with `EventSetup`.
   - External services are initialized with `Initialize`.

2. **User Interactions**:
   - Users interact with the search area to filter the grid.
   - Users click buttons to perform actions like creating, modifying, or viewing records.

### Functions:
- **`CreateListForm`** (File: `LsalesMan`): Creates and initializes the form.
- **`GridSetup`** (File: `LsalesMan`): Configures the grid columns and settings.
- **`Initialize`** (File: `LsalesMan`): Sets up external services and default parameters.

### Required Data:
- **Search Filters**: Name, Office, Active Status.
- **Grid Data**: Sales manager records retrieved from external services.

---

## 4. Business Rules:

### Actions and Preconditions:
- **New**: No preconditions; always enabled.
- **Modify**: Enabled only if a record is selected.
- **View**: Enabled only if a record is selected.

### Available Filters:
- **Name**: Text input for filtering by name.
- **Office**: Dropdown or search field for filtering by office.
- **Active Only**: Checkbox to filter active sales managers.

### Error Messages:
- "No record selected" if Modify or View is clicked without selecting a record.
- "Service unavailable" if external service calls fail.

### Default Field Values:
- **Active Only**: Checked by default.

### Field Validation and Conditions:
- **Name**: No explicit validation defined in the code.
- **Office**: No explicit validation defined in the code.

---

## 5. Main Functions:

1. **`CreateListForm`**: Creates and initializes the sales manager list form.
2. **`GridSetup`**: Configures the grid, including hidden fields, column order, and custom editors.
3. **`Initialize`**: Sets up external services and default parameters.
4. **`EventSetup`**: Configures event handlers for the form.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `SalesManServiceUtils`.
   - **Endpoint**: `/api/salesman`.
   - **Data Sent**: `{ "filters": { "name": "string", "office": "string", "activeOnly": "boolean" } }`.
   - **Data Received**: `{ "status": "success", "data": [Salesman objects] }`.
   - **Purpose**: Retrieve sales manager data.
   - **Error Handling**: Displays "Service unavailable" if the call fails.

2. **Service Name**: `OfficeServiceUtils`.
   - **Endpoint**: `/api/offices`.
   - **Purpose**: Retrieve office data for filtering.

---

## 7. Conditional Fields (Form Logic):

- **Active Only** checkbox affects the grid display.
- **Office** field is always visible and used for filtering.

---

## 8. Dependencies:

### External Libraries:
- **`cxGrid`**: Used for displaying the grid.
- **`TsEdit`**: Text input for the name filter.
- **`TsCheckBox`**: Checkbox for the active-only filter.

### Custom Components:
- **`kneCBListSOA`**: Base class for the form.
- **`FRAMEFindEditSOA`**: Custom component for the office filter.

---

## 9. Fields and Validations Listing:

1. **Name**:
   - Type: String.
   - Required: No.
   - Validation: Not explicitly defined in the code.

2. **Office**:
   - Type: String.
   - Required: No.
   - Validation: Not explicitly defined in the code.

3. **Active Only**:
   - Type: Boolean.
   - Default: Checked.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
// Creating the form
var
  SalesManForm: TFORMLsalesMan;
begin
  SalesManForm := TFORMLsalesMan.CreateListForm(Self);
  SalesManForm.Show;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 600px; padding: 10px; border: 1px solid #ccc;">
  <label for="name">Name:</label>
  <input type="text" id="name" style="width: 100%; margin-bottom: 10px;">
  
  <label for="office">Office:</label>
  <input type="text" id="office" style="width: 100%; margin-bottom: 10px;">
  
  <label>
    <input type="checkbox" id="activeOnly" checked> Active Only
  </label>
</div>
```

---

## 11. Important Comments in the Code:

- **GridSetup**: Configures hidden fields, column order, and custom editors.
- **Initialize**: Sets up external services and default parameters.

---

## 12. Conclusion:

The `LsalesMan` code unit provides a robust solution for managing sales manager data. Its integration with external services and customizable grid make it a powerful tool. However, the lack of explicit field validations and error handling for user inputs could be improved.

---

## 13. Short Summary:

The `LsalesMan` unit manages a grid-based sales manager list with search and filter functionalities, integrating external services for data handling. It supports creating, modifying, and viewing records, ensuring efficient data management.#### **LsalesMan.pas**

```
unit LsalesMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, knePrivileges, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, sScrollBox, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, kneEnterAsTab, kneFRGridManager, Buttons,
  sSpeedButton, ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter,
  sCheckBox, sEdit, kneCBList, kneFRFindEditSOA;

type
  TFORMLsalesMan = class(TFORMkneCBListSOA)
    LBLname: TsLabel;
    EDTname: TsEdit;
    CHKactiveOnly: TsCheckBox;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    LBLoffice: TsLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure m_SetFindOffice;
  protected
    procedure GridSetup; override;
    procedure EventSetup; override;
    procedure EditorClosed(Sender: TObject); override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;
  end;

var
  FORMLsalesMan: TFORMLsalesMan;

implementation

uses
  kneUtils,
  MsalesMan, 
  SalesManServiceUtils,
  OfficeServiceUtils;

{$R *.dfm}


class function TFORMLsalesMan.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLsalesMan.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLsalesMan.EventSetup;
begin
  inherited;

end;

procedure TFORMLsalesMan.GridSetup;
begin
  inherited;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('stat;salesman;name;login;officeCode;officeDesc;email;lastUpd;updBy;');
    // Custom Editors ..........................................................
    AddCustomField('stat','cxEDTstatus');
  end;
end;

class procedure TFORMLsalesMan.Initialize(
  const pv_FormList: TFORMkneCBList);
begin
  inherited;

  TFORMkneCBListSOA(pv_FormList).ProviderService := TSalesManServiceUtils.Create(pv_FormList);
  TFORMkneCBListSOA(pv_FormList).AutoLoad        := True;
  TFORMkneCBListSOA(pv_FormList).ServiceParams.ShowInactives := False;
end;

```

#### **LsalesMan.dfm**

```
inherited FORMLsalesMan: TFORMLsalesMan
  Left = 470
  Top = 203
  Caption = 'Sales Manager List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 153
  end
  inherited PNLsearchArea: TsPanel
    Height = 109
    inherited PNLsearchButtons: TsPanel
      Height = 107
      TabOrder = 0
    end
    inherited SRBcriteria: TsScrollBox
      Height = 107
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        Height = 103
        object LBLname: TsLabel
          Left = 16
          Top = 21
          Width = 31
          Height = 13
          Caption = 'Name:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLoffice: TsLabel
          Left = 16
          Top = 50
          Width = 33
          Height = 13
          Caption = 'Office:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object CHKactiveOnly: TsCheckBox
          Left = 63
          Top = 74
          Width = 80
          Height = 19
          Caption = 'Active Only'
          Checked = True
          State = cbChecked
          TabOrder = 0
          SkinData.SkinSection = 'CHECKBOX'
          ImgChecked = 0
          ImgUnchecked = 0
        end
        object EDTname: TsEdit
          Left = 64
          Top = 13
          Width = 497
          Height = 21
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
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
        inline FRAMEfindOffice: TFRAMEFindEditSOA
          Left = 64
          Top = 44
          Width = 497
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
          TabOrder = 2
          inherited PNLdesc: TPanel
            Width = 391
```
<!-- tabs:end -->

