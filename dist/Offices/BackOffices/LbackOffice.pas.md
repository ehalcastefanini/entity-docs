<!-- tabs:start -->

#### **Documentation**

# Documentation for `LbackOffice` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `LbackOffice` code unit is designed to manage and display a list of back-office records in a grid format. It provides functionalities for viewing, editing, and managing back-office data, such as descriptions, statuses, and update information. The main objective is to offer a user-friendly interface for interacting with back-office data, including search and advanced filtering options.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Database Integration**: The code interacts with a database using data-aware components like `TsDBText` and `DSRlist`.
- **Custom Components**: Includes custom components like `kneCBListSOA`, `knePrivileges`, and `kneFRGridManager`.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **status**: Displays the status of the back-office record (string).
2. **backoffice**: Represents the back-office identifier (string).
3. **boDescrip**: Description of the back-office (string).
4. **respons**: Responsible person or entity (string).
5. **respName**: Name of the responsible person (string).
6. **updBy**: User who last updated the record (string).
7. **lastUpd**: Timestamp of the last update (datetime).

#### Grid Actions and Their Effects:
1. **New**: Creates a new back-office record.
2. **Modify**: Edits an existing back-office record.
3. **View**: Views details of a selected back-office record.
4. **Search Area**: Allows searching for specific records.
5. **Advanced Search**: Provides advanced filtering options.

---

## 2. Functionality Description:

### User/Software Actions:
- View back-office records in a grid.
- Perform CRUD (Create, Read, Update, Delete) operations.
- Search and filter records using basic and advanced search options.

### Main Components:
1. **Grid**: Displays the list of back-office records.
2. **Action List**: Manages user actions like creating, modifying, and viewing records.
3. **Data-Aware Components**: Bind UI elements to database fields for real-time data display.

### Pseudo-Code for Actions and Events:
- **OnClick event of "New" button**: `if "New" button clicked then open editor for new record`.
- **OnClick event of "Modify" button**: `if "Modify" button clicked then open editor for selected record`.
- **OnClick event of "View" button**: `if "View" button clicked then display details of selected record`.
- **OnChange event of search field**: `if search field value changed then filter grid records`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `CreateListForm`.
   - The grid is set up with specific columns and custom editors in `GridSetup`.
   - Event handlers are configured in `EventSetup`.

2. **User Interactions**:
   - Users interact with the grid and buttons to perform actions like creating, modifying, or viewing records.

3. **Functions**:
   - **`CreateListForm`** (File: `LbackOffice`): Creates and initializes the form.
   - **`GridSetup`** (File: `LbackOffice`): Configures the grid columns and settings.
   - **`EventSetup`** (File: `LbackOffice`): Sets up event handlers.

### Required Data:
- Users must provide data for fields like `status`, `boDescrip`, `respons`, and `respName` to create or modify records.

---

## 4. Business Rules:

### Actions and Preconditions:
- **New**: Enabled at all times.
- **Modify**: Enabled only when a record is selected.
- **View**: Enabled only when a record is selected.

### Available Filters:
- **Search Area**: Basic search functionality.
- **Advanced Search**: Allows filtering by specific fields like `status`, `boDescrip`, etc.

### Error Messages:
- "No record selected" if attempting to modify or view without selecting a record.
- "Invalid data" if required fields are not filled or contain invalid values.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **status**: Should be a valid status string.
- **boDescrip**: Should not be empty.
- **lastUpd**: Should be a valid datetime.

---

## 5. Main Functions:

1. **`CreateListForm`**: Creates and initializes the back-office list form.
2. **`GridSetup`**: Configures the grid, including hidden fields, column order, and custom editors.
3. **`EventSetup`**: Sets up event handlers for user interactions.
4. **`Initialize`**: Initializes the form with a provider service.

---

## 6. API Service Consumption:

- **Service Name**: `BackOfficeServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Depends on the operation (e.g., new record data, modified record data).
- **Data Received**: Back-office records or operation status.
- **Purpose**: Manage back-office records.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components**: For UI elements.
- **Custom Components**: `kneCBListSOA`, `knePrivileges`, `kneFRGridManager`.

### Custom Components:
- **`kneCBListSOA`**: Base class for list forms.
- **`kneFRGridManager`**: Manages grid functionalities.

---

## 9. Fields and Validations Listing:

1. **status**: (type: string, required).
2. **boDescrip**: (type: string, required).
3. **respons**: (type: string, optional).
4. **respName**: (type: string, optional).
5. **updBy**: (type: string, system-generated).
6. **lastUpd**: (type: datetime, system-generated).

Mapping of displayed values to database columns:
- `status` → `status`.
- `boDescrip` → `boDescrip`.
- `respons` → `respons`.
- `respName` → `respName`.
- `updBy` → `updBy`.
- `lastUpd` → `lastUpd`.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
var
  BackOfficeForm: TFORMLbackOffice;
begin
  BackOfficeForm := TFORMLbackOffice.CreateListForm(Self);
  BackOfficeForm.Show;
end;
```

### Screenshots:
HTML representation of the grid:
```html
<table style="width:100%; border:1px solid black;">
  <thead>
    <tr>
      <th>Status</th>
      <th>Back Office</th>
      <th>Description</th>
      <th>Responsible</th>
      <th>Updated By</th>
      <th>Last Updated</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Active</td>
      <td>BO001</td>
      <td>Finance</td>
      <td>John Doe</td>
      <td>Admin</td>
      <td>2023-10-01</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **GridSetup**: Configures grid columns and custom editors.
- **EventSetup**: Sets up event handlers for user interactions.

---

## 12. Conclusion:

The `LbackOffice` code unit provides a robust framework for managing back-office records. Its strengths include modularity, database integration, and customizability. However, it lacks detailed error handling and API endpoint definitions.

---

## 13. Short Summary:

The `LbackOffice` code unit manages back-office records using a grid interface, supporting CRUD operations and search functionalities. It integrates with a database and utilizes custom components for enhanced functionality.#### **LbackOffice.pas**

```
unit LbackOffice;

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
  sDBText, kneCBList;

type
  TFORMLbackOffice = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    EDTdescrip: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    EDTsalesdir: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
  private
    { Private declarations }
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
  FORMLbackOffice: TFORMLbackOffice;

implementation

uses
  kneUtils,
  MbackOffice,
  BackOfficeServiceUtils;

{$R *.dfm}

class function TFORMLbackOffice.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLbackOffice.Create(AOwner);

  Initialize(Result);

end;                                 

procedure TFORMLbackOffice.EventSetup;
begin
  inherited;

end;

procedure TFORMLbackOffice.GridSetup;
begin
  inherited;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
//    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('status; backoffice; boDescrip; respons; respName; updBy;'+
      ' lastUpd');
    // Custom Editors ..........................................................
    AddCustomField('status','cxEDTstatus');
  end;

  ShowSearchArea := False;
end;

class procedure TFORMLbackOffice.Initialize(
  const pv_FormList: TFORMkneCBList);
begin
  inherited;

  TFORMkneCBListSOA(pv_FormList).ProviderService := TBackOfficeServiceUtils.Create(pv_FormList);
```

#### **LbackOffice.dfm**

```
inherited FORMLbackOffice: TFORMLbackOffice
  Left = 404
  Top = 141
  Caption = 'Back Offices List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLlist: TsPanel
    inherited PNLdetailArea: TsPanel
      inherited PNLviewer: TsScrollBox
        object LBL1: TsLabel
          Left = 8
          Top = 16
          Width = 71
          Height = 16
          Caption = 'Back Office'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        object BVL2: TsBevel
          Left = 8
          Top = 35
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
        object EDTdescrip: TsDBText
          Left = 24
          Top = 72
          Width = 53
          Height = 13
          Caption = 'Description'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'boDescrip'
          DataSource = DSRlist
        end
        object sLabel1: TsLabel
          Left = 8
          Top = 96
          Width = 43
          Height = 16
          Caption = 'Status'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        object sBevel2: TsBevel
          Left = 7
          Top = 115
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
        object DBLstat: TsDBText
          Left = 24
          Top = 128
          Width = 31
          Height = 13
          Caption = 'Status'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'status'
          DataSource = DSRlist
        end
        object DBLlastUpd: TsDBText
          Left = 24
          Top = 152
          Width = 46
          Height = 13
          Caption = 'Last Upd.'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'lastUpd'
          DataSource = DSRlist
        end
        object DBLupdBy: TsDBText
          Left = 147
          Top = 152
```
<!-- tabs:end -->

