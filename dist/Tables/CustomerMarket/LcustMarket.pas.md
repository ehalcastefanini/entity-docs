<!-- tabs:start -->

#### **Documentation**

# Documentation for `LcustMarket` Code Unit

## 1. Overview:

### Objective:
The `LcustMarket` code unit is designed to manage and display a list of customer markets in a grid format. It provides functionalities for viewing, searching, and managing customer market data. The main objective is to offer a user-friendly interface for interacting with customer market records, including filtering, sorting, and performing actions like creating, modifying, and viewing details.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Database Integration**: The code interacts with a database using `DBClient` and `DataSource` components.
- **Third-party Libraries**: Includes components like `TsLabel`, `TsBevel`, `TsDBText`, and `cxGrid` for enhanced UI and grid functionalities.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **stat**: Status (Custom Field).
2. **marketCode**: Market Code (String).
3. **description**: Description (String).
4. **currency**: Currency (String).
5. **regionCode**: Region Code (String).
6. **region**: Region (String).

#### Grid Actions and Their Effects:
1. **New**: Opens a form to create a new customer market record.
2. **Modify**: Opens a form to edit the selected customer market record.
3. **View**: Opens a form to view details of the selected customer market record.
4. **Search Area**: Allows users to search within a specific area.
5. **Advanced Search**: Provides advanced filtering options for the grid.

---

## 2. Functionality Description:

### User/Software Actions:
- View a list of customer markets in a grid.
- Perform actions like creating, modifying, and viewing records.
- Search and filter customer market data using basic and advanced criteria.

### Main Components:
1. **Grid (`cxGrid`)**: Displays the list of customer markets.
2. **Search Panel (`PNLsearchArea`)**: Provides search and filtering options.
3. **Detail Panel (`PNLdetailArea`)**: Displays detailed information about the selected record.
4. **Action List (`ACLeditingActions_deriv`)**: Manages actions like New, Modify, View, etc.

### Pseudo-code for Actions and Events:
- **OnClick event of "New" button**: `if New button clicked then open form to create a new record`.
- **OnClick event of "Modify" button**: `if Modify button clicked then open form to edit the selected record`.
- **OnClick event of "View" button**: `if View button clicked then open form to view details of the selected record`.
- **OnChange event of search criteria**: `if search criteria changed then refresh grid with filtered data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized using the `CreateListForm` method.
   - The `GridSetup` and `EventSetup` methods are called to configure the grid and events.
2. **User Interaction**:
   - Users interact with the grid and perform actions like New, Modify, View, or search.
   - Actions trigger corresponding methods to handle the functionality.

### Functions and File References:
1. **`CreateListForm`** (File: `LcustMarket`):
   - Creates and initializes the form.
2. **`GridSetup`** (File: `LcustMarket`):
   - Configures the grid, including hidden fields and custom fields.
3. **`EventSetup`** (File: `LcustMarket`):
   - Sets up event handlers for user interactions.

### Required Data:
- Users must provide search criteria or select a record to perform actions like Modify or View.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **New**: No preconditions; always enabled.
2. **Modify**: Enabled only if a record is selected.
3. **View**: Enabled only if a record is selected.
4. **Search Area**: No preconditions; always enabled.
5. **Advanced Search**: No preconditions; always enabled.

### Available Filters:
- Basic search criteria (e.g., market code, description).
- Advanced search options (not explicitly detailed in the code).

### Error Messages:
- "No record selected" if Modify or View is clicked without selecting a record.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

1. **`CreateListForm`**:
   - Creates and initializes the customer market list form.
2. **`GridSetup`**:
   - Configures the grid, including hidden fields, order fields, and custom fields.
3. **`EventSetup`**:
   - Sets up event handlers for user interactions.

---

## 6. API Service Consumption:

- **Service Name**: `CustomerMarketServiceUtils`.
- **Purpose**: Likely used for fetching and managing customer market data (details not explicitly provided in the code).

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **`TsLabel`, `TsBevel`, `TsDBText`**: Used for UI components.
- **`cxGrid`**: Used for displaying the grid.
- **`DBClient`**: Used for database interaction.

### Custom Components:
- **`FRAMEfindCriteriaCodeDesc`**: A custom frame for search criteria.

---

## 9. Fields and Validations Listing:

### Fields in the Grid:
1. **stat**: Custom field (type: string, not explicitly validated).
2. **marketCode**: Market Code (type: string, not explicitly validated).
3. **description**: Description (type: string, not explicitly validated).
4. **currency**: Currency (type: string, not explicitly validated).
5. **regionCode**: Region Code (type: string, not explicitly validated).
6. **region**: Region (type: string, not explicitly validated).
7. **lastUpd**: Last Updated (type: string, not explicitly validated).
8. **updBy**: Updated By (type: string, not explicitly validated).
9. **regionCode**: Region Code (type: string, not explicitly validated).
10. **region**: Region (type: string, not explicitly validated).
11. **currency**: Currency (type: string, not explicitly validated).

### Mapping:
- **Database Columns**: `stat`, `marketCode`, `description`, `currency`, `regionCode`, `region`.
- **Displayed Values**: Same as database columns.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [Load Grid Data] --> [User Interaction] --> [Perform Action] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
Form --> Grid: Load Data
User --> Grid: Select Record
User --> Form: Click Action (e.g., Modify)
Form --> Database: Fetch/Update Data
```

### Code Snippets:
```delphi
// Create and initialize the form
var
  Form: TFORMLcustMarket;
begin
  Form := TFORMLcustMarket.CreateListForm(Self);
  Form.Show;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`GridSetup`**: Configures the grid, including hidden fields and custom fields.
- **`CreateListForm`**: Initializes the form and sets up the grid and events.

---

## 12. Conclusion:

The `LcustMarket` code unit provides a robust framework for managing customer market data in a grid format. It supports essential actions like creating, modifying, and viewing records, along with search and filtering capabilities. However, the code lacks explicit field validations and error handling, which could be improved for better user experience.

---

## 13. Short Summary:

The `LcustMarket` code unit manages customer market data in a grid format, offering functionalities like creating, modifying, viewing, and searching records. It integrates with a database and provides a user-friendly interface for efficient data management.

#### **LcustMarket.pas**

```
unit LcustMarket;

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
  sDBText, kneFRfindCriteria, kneFRfindCriteriaCodeDesc, sScrollBox,
  kneEnterAsTab, knePrivileges;

type
  TFORMLcustMarket = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTmarketCode: TsDBText;
    DBTXTdescription: TsDBText;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBTXTregionCode: TsDBText;
    DBTXTregion1: TsDBText;
    DBTXTcurrency: TsDBText;
    FRAMEfindCriteriaCodeDesc1: TFRAMEfindCriteriaCodeDesc;
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

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLcustMarket: TFORMLcustMarket;

implementation

uses
  CustomerMarketServiceUtils, McustMarket;

{$R *.dfm}

{ TFORMLcustMarket }

class function TFORMLcustMarket.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLcustMarket.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLcustMarket.EventSetup;
begin
  inherited;

end;

procedure TFORMLcustMarket.GridSetup;
begin
  inherited;

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields('stat; marketCode; description; currency; regionCode; region');

    AddCustomField('stat','cxEDTstatus');
  end;

end;

class procedure TFORMLcustMarket.Initialize(
  const pv_FormList: TFORMkneCBList);
begin
  inherited;

  TFORMkneCBListSOA(pv_FormList).ProviderService := TCustomerMarketServiceUtils.Create(pv_FormList);
  //TFORMkneCBListSOA(pv_FormList).EditorForm      := TFORMMcustMarket.Create(pv_FormList);
  TFORMkneCBListSOA(pv_FormList).AutoLoad        := True;
  TFORMkneCBListSOA(pv_FormList).ServiceParams.ShowInactives := True;
end;

function TFORMLcustMarket.SetupParams: Boolean;
begin
  // Atribui��o dos Fields do c�digo e da descri��o usados no servi�o
  FRAMEfindCriteriaCodeDesc1.FieldCode := 'marketCode';
  FRAMEfindCriteriaCodeDesc1.FieldDescription := 'description';

  // Atribui��o dos crit�rios ao servi�o
  ServiceParams.Criteria := FRAMEfindCriteriaCodeDesc1.CriteriaValues;

  Result := inherited SetupParams;
end;

procedure TFORMLcustMarket.CreateEditor;
begin
  inherited;
  EditorForm := TFORMMcustMarket.Create(Self);
end;

end.

```

#### **LcustMarket.dfm**

```
inherited FORMLcustMarket: TFORMLcustMarket
  Left = 170
  Top = 147
  Caption = 'Customer Market List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLsearchArea: TsPanel
    inherited PNLsearchButtons: TsPanel
      TabOrder = 0
    end
    inherited SRBcriteria: TsScrollBox
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        inline FRAMEfindCriteriaCodeDesc1: TFRAMEfindCriteriaCodeDesc
          Left = 1
          Top = 1
          Width = 689
          Height = 64
          Align = alClient
          ParentBackground = False
          TabOrder = 0
        end
      end
    end
  end
  inherited PNLlist: TsPanel
    inherited PNLdetailArea: TsPanel
      inherited PNLviewer: TsScrollBox
        object LBL1: TsLabel
          Left = 8
          Top = 16
          Width = 46
          Height = 16
          Caption = 'Market'
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
        object sLabel1: TsLabel
          Left = 8
          Top = 200
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
          Top = 219
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
        object DBTXTmarketCode: TsDBText
          Left = 24
          Top = 48
          Width = 25
          Height = 13
          Caption = 'Code'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'marketCode'
          DataSource = DSRlist
        end
        object DBTXTdescription: TsDBText
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
          DataField = 'description'
          DataSource = DSRlist
        end
        object DBLstat: TsDBText
          Left = 24
          Top = 232
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
          DataField = 'stat'
          DataSource = DSRlist
        end
        object DBLlastUpd: TsDBText
          Left = 24
          Top = 256
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
          Left = 112
          Top = 256
          Width = 38
          Height = 13
          Caption = 'Upd. By'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'updBy'
          DataSource = DSRlist
        end
        object sLabel2: TsLabel
          Left = 8
          Top = 120
          Width = 44
          Height = 16
          Caption = 'Region'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        object sBevel3: TsBevel
          Left = 7
          Top = 139
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
        object DBTXTregionCode: TsDBText
          Left = 24
          Top = 152
          Width = 25
          Height = 13
          Caption = 'Code'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'regionCode'
          DataSource = DSRlist
        end
        object DBTXTregion1: TsDBText
          Left = 24
          Top = 176
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
          DataField = 'region'
          DataSource = DSRlist
        end
        object DBTXTcurrency: TsDBText
          Left = 24
          Top = 96
          Width = 44
          Height = 13
          Caption = 'Currency'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'currency'
          DataSource = DSRlist
        end
      end
    end
  end
  object ACLeditingActions_deriv: TActionList
    Left = 56
    Top = 216
    object ACTnew_deriv: TAction
      Tag = 1
      Category = 'Edit'
      Caption = '&New'
      Enabled = False
      Visible = False
    end
    object ACTmodify_deriv: TAction
      Tag = 2
      Category = 'Edit'
      Caption = '&Modify'
      Enabled = False
      Visible = False
    end
    object ACTview_deriv: TAction
      Tag = 3
      Category = 'Edit'
      Caption = '&View'
      Enabled = False
      Visible = False
    end
    object ACTsearchArea_deriv: TAction
      Category = 'Search'
      Caption = 'Searc&h'
      Enabled = False
      Visible = False
    end
    object ACTadvancedSearch_deriv: TAction
      Category = 'Search'
      Caption = '&Advanced'
      Enabled = False
      Visible = False
    end
  end
end

```
<!-- tabs:end -->

