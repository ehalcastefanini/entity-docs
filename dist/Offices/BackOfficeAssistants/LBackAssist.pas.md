<!-- tabs:start -->

#### **Documentation**

# Documentation for `LBackAssist` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `LBackAssist` code unit defines a form (`TFORMLBackAssist`) for managing and displaying a list of Back Office Assistants. It provides a user interface for searching, filtering, and interacting with Back Office Assistant data. The form includes search criteria fields, a grid to display results, and actions for creating, modifying, and viewing records. This form is part of a larger system for managing Back Office operations.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **Third-party Libraries**:
  - `TsLabel`, `TsEdit`, `TsCheckBox`, `TsBitBtn`, `TsPanel`, `TsSplitter`: Components from the AlphaControls library for enhanced UI styling.
  - `cxGrid`, `cxGridDBTableView`: DevExpress components for grid display and data binding.
  - `kneCBListSOA`, `kneFRFindEditSOA`: Custom components for specific functionalities like search and filtering.

### Form Type:
This is a **form** with the following elements:
- **Form Elements**:
  - `EDTname` (Text Input): For entering the name of the Back Office Assistant.
  - `CHKactiveOnly` (Checkbox): For filtering active assistants only.
  - `FRAMEfindOffice` (Custom Component): For selecting an office.
- **Form Actions**:
  - `ACTnew_deriv`: Action to create a new record.
  - `ACTmodify_deriv`: Action to modify an existing record.
  - `ACTview_deriv`: Action to view details of a record.
  - `ACTsearchArea_deriv`: Action to search within a specific area.
  - `ACTadvancedSearch_deriv`: Action to perform an advanced search.

---

## 2. Functionality Description:

### User/Software Actions:
- Search for Back Office Assistants using name, office, and active status filters.
- View, create, or modify Back Office Assistant records.
- Perform advanced searches or area-specific searches.

### Main Components:
- **Search Area Panel (`PNLsearchArea`)**: Contains search criteria fields and buttons.
- **Grid (`cxGrid`)**: Displays the list of Back Office Assistants.
- **Action List (`ACLeditingActions_deriv`)**: Manages user actions like creating, modifying, and viewing records.

### Pseudo-code for Actions and Events:
- `OnClick` event of `BTsearch`: `if search button clicked then execute search function`.
- `OnClick` event of `BTclearCriteria`: `if clear button clicked then reset all search fields`.
- `OnChange` event of `CHKactiveOnly`: `if checkbox value changed then update filter`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `TFORMLBackAssist.CreateListForm`.
   - `GridSetup` and `EventSetup` methods are called to configure the grid and events.
2. **User Interaction**:
   - Users fill in search criteria (e.g., name, office, active status).
   - Users click the search button to filter results.
   - Users can select a record from the grid to view, modify, or create new records.

### Data Input:
- **Name**: Text input for filtering by name.
- **Office**: Selected using the `FRAMEfindOffice` component.
- **Active Only**: Checkbox to filter active records.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search Button**: Enabled when at least one search criterion is filled.
- **Clear Button**: Clears all search fields and resets filters.
- **Create/Modify/View Actions**: Require a record to be selected in the grid.

### Available Filters:
- **Name**: Text-based filter.
- **Office**: Dropdown or search-based filter.
- **Active Only**: Checkbox filter.

### Error Messages:
- "No records found" if the search yields no results.
- "Please select a record" if an action is attempted without selecting a record.

### Default Field Values:
- `CHKactiveOnly`: Default is checked (`True`).

### Field Validation and Conditions:
- **Name**: Must be a string; no specific validation defined in the code.
- **Office**: Must be selected; no specific validation defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`CreateListForm`**:
   - Creates and initializes the form.
2. **`GridSetup`**:
   - Configures the grid, including hidden fields, field order, and custom editors.
3. **`EventSetup`**:
   - Sets up event handlers for user interactions.
4. **`SetupParams`**:
   - Prepares parameters for filtering and searching.

---

## 6. API Service Consumption:

- **Service Name**: `BackOfficeServiceUtils`
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely used for fetching and updating Back Office Assistant data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **"Active Only" Checkbox**: Filters results based on active status.
- **"Office" Field**: Appears as part of the search criteria.

---

## 8. Dependencies:

### External Libraries:
- **AlphaControls**: For enhanced UI components.
- **DevExpress**: For grid and data display components.

### Custom Components:
- **`kneCBListSOA`**: Base class for the form.
- **`kneFRFindEditSOA`**: Custom component for office selection.

---

## 9. Fields and Validations Listing:

- **Name (`EDTname`)**: Type: string, optional, no validation defined.
- **Office (`FRAMEfindOffice`)**: Type: custom component, optional, no validation defined.
- **Active Only (`CHKactiveOnly`)**: Type: boolean, default: checked.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Form Initialization] --> [Grid Setup] --> [Event Setup]
    --> [User Inputs Search Criteria] --> [Search Button Clicked]
    --> [Filter Results in Grid]
```

### Sequence Diagram:
```plaintext
User --> Form: Inputs search criteria
User --> Form: Clicks search button
Form --> BackOfficeServiceUtils: Fetch filtered data
BackOfficeServiceUtils --> Form: Returns data
Form --> User: Displays results in grid
```

### Code Snippets:
```delphi
procedure TFORMLBackAssist.GridSetup;
begin
  inherited;
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('status;login;boAssist;name;tpFunc;backOffice;email;lastUpd;updBy;');
    AddCustomField('status', 'cxEDTstatus');
  end;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Verdana; width: 600px;">
  <div style="margin-bottom: 10px;">
    <label for="name">Name:</label>
    <input type="text" id="name" style="width: 300px;" />
    <input type="checkbox" id="activeOnly" checked /> Active Only
  </div>
  <div>
    <label for="office">Office:</label>
    <input type="text" id="office" style="width: 300px;" />
  </div>
  <button>Search</button>
  <button>Clear</button>
</div>
```

---

## 11. Important Comments in the Code:

- `GridSetup`: Configures the grid, including hidden fields and custom editors.
- `CreateListForm`: Initializes the form and sets up parameters.

---

## 12. Conclusion:

The `LBackAssist` code unit provides a robust interface for managing Back Office Assistants. Its strengths include modularity, reusable components, and a clear separation of concerns. However, the lack of explicit API details and error handling could be improved.

---

## 13. Short Summary:

The `LBackAssist` form manages Back Office Assistants, offering search, filter, and CRUD functionalities. It uses AlphaControls and DevExpress for UI and grid management, ensuring a user-friendly interface for Back Office operations.#### **LBackAssist.pas**

```
unit LBackAssist;

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
  sCheckBox, sEdit, kneCBList, kneFRFindEditSOA, sDBText;

type
  TFORMLBackAssist = class(TFORMkneCBListSOA)
    LBLname: TsLabel;
    EDTname: TsEdit;
    CHKactiveOnly: TsCheckBox;
    FRAMEfindOffice: TFRAMEFindEditSOA;
    LBLoffice: TsLabel;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    LBL1: TsLabel;
    BVL2: TsBevel;
    EDTdescrip: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    EDTsalesdir: TsDBText;
    procedure FormCreate(Sender: TObject);
  private
    procedure m_SetFindBackOffice;
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
  FORMLBackAssist: TFORMLBackAssist;

implementation

uses
  kneUtils,
  MbackAssist,
  BackOfficeServiceUtils,
  BoAssistServiceUtils;

{$R *.dfm}

class function TFORMLBackAssist.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLBackAssist.Create(AOwner);

  Initialize(Result);

end;

procedure TFORMLBackAssist.EventSetup;
begin
  inherited;

end;

procedure TFORMLBackAssist.GridSetup;
begin
  inherited;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('status;login;boAssist;name;tpFunc;backOffice;email;lastUpd;updBy;');
    // Custom Editors ..........................................................
    AddCustomField('status','cxEDTstatus');
  end;

  BTNsearchArea.Click;        // esconde a janela dos crit�rios
end;
```

#### **LBackAssist.dfm**

```
inherited FORMLBackAssist: TFORMLBackAssist
  Left = 334
  Top = 178
  Caption = 'Back Office Assistants List'
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 125
  end
  inherited PNLsearchArea: TsPanel
    Height = 81
    inherited PNLsearchButtons: TsPanel
      Height = 79
      TabOrder = 0
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Height = 79
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        Height = 75
        object LBLname: TsLabel
          Left = 8
          Top = 13
          Width = 38
          Height = 13
          Caption = 'Name:'
        end
        object LBLoffice: TsLabel
          Left = 8
          Top = 39
          Width = 38
          Height = 13
          Caption = 'Office:'
        end
        object EDTname: TsEdit
          Left = 64
          Top = 8
          Width = 387
          Height = 21
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
        object CHKactiveOnly: TsCheckBox
          Left = 480
          Top = 8
          Width = 90
          Height = 19
          Caption = 'Active Only'
          Checked = True
          State = cbChecked
          TabOrder = 1
          SkinData.SkinSection = 'CHECKBOX'
          ImgChecked = 0
          ImgUnchecked = 0
        end
        inline FRAMEfindOffice: TFRAMEFindEditSOA
          Left = 64
          Top = 34
          Width = 497
          Height = 21
          HorzScrollBar.Visible = False
          VertScrollBar.Visible = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          inherited PNLdesc: TPanel
            Width = 391
            DesignSize = (
              391
              21)
            inherited DBEDesc: TsDBEdit
              Width = 391
            end
            inherited EDDesc: TsEdit
              Width = 391
            end
          end
        end
```
<!-- tabs:end -->

