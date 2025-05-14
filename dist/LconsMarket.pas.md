<!-- tabs:start -->

#### **Documentation**

# Documentation for `LconsMarket` Code Unit

---

## 1. Overview:

### Objective:
The `LconsMarket` code unit is designed to manage and display a list of "Consignee Markets" in a grid format. It provides functionalities for viewing, searching, and interacting with market data. The primary objective is to allow users to efficiently manage and view market-related information, such as market codes, descriptions, regions, and statuses.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **Database Components**: For interacting with the database (e.g., `DBClient`, `TsDBText`).
- **Third-party Libraries**: Includes `kneCBListSOA`, `kneFRfindCriteria`, and `knePrivileges` for extended functionalities like search criteria and privilege management.

### Form Type:
This code represents a **grid display**. 

#### Grid Columns and Their Types:
1. **stat**: Status (Custom Field).
2. **marketCode**: Market Code (String).
3. **description**: Description (String).
4. **regionCode**: Region Code (String).
5. **region**: Region (String).

#### Grid Actions and Their Effects:
1. **New**: Allows the creation of a new market entry.
2. **Modify**: Enables editing of an existing market entry.
3. **View**: Opens a detailed view of a selected market entry.
4. **Search Area**: Filters the grid based on specific criteria.
5. **Advanced Search**: Provides advanced filtering options.

---

## 2. Functionality Description:

### User/Software Actions:
- View a list of markets in a grid.
- Perform CRUD (Create, Read, Update, Delete) operations on market data.
- Search and filter market data using basic and advanced criteria.

### Main Components:
1. **Grid Display**: Displays market data in a tabular format.
2. **Search Panel**: Allows users to filter data using search criteria.
3. **Action Buttons**: Provide options for creating, modifying, viewing, and searching data.

### Pseudo-code for Actions and Events:
- **OnClick event of "New" button**: `if New button clicked then open form for new market entry`.
- **OnClick event of "Modify" button**: `if Modify button clicked then open form to edit selected market entry`.
- **OnClick event of "View" button**: `if View button clicked then open form to view selected market entry`.
- **OnChange event of search criteria**: `if search criteria changed then refresh grid with filtered data`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized using the `CreateListForm` method.
   - The grid is set up with hidden fields, order fields, and custom fields in the `GridSetup` method.
   - Event handlers are configured in the `EventSetup` method.

2. **User Interactions**:
   - Users interact with the grid and action buttons to perform operations.
   - Search criteria are applied to filter the grid.

### Functions:
- **`CreateListForm`** (File: `LconsMarket`): Creates and initializes the form.
- **`GridSetup`** (File: `LconsMarket`): Configures the grid display.
- **`EventSetup`** (File: `LconsMarket`): Sets up event handlers.

### Data Requirements:
- Users must provide valid search criteria or select a grid row to perform actions like Modify or View.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **New**: No preconditions; opens a blank form for data entry.
2. **Modify**: Requires a row to be selected in the grid.
3. **View**: Requires a row to be selected in the grid.
4. **Search Area**: Requires valid search criteria.
5. **Advanced Search**: Requires advanced search parameters.

### Available Filters:
- **Basic Filters**: Market Code, Description, Region Code.
- **Advanced Filters**: Not explicitly defined in the code.

### Error Messages:
- "No row selected" if Modify or View is clicked without selecting a row.
- "Invalid search criteria" if search parameters are invalid.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Market Code**: Must be unique and non-empty.
- **Description**: Should not exceed a certain length (not defined in the code).
- **Region Code**: Must match a valid region.

---

## 5. Main Functions:

1. **`CreateListForm`**: Initializes the form and sets up the grid.
2. **`GridSetup`**: Configures the grid with hidden fields, order fields, and custom fields.
3. **`EventSetup`**: Sets up event handlers for user interactions.
4. **`Initialize`**: Prepares the form for use.

---

## 6. API Service Consumption:

- **Service Name**: `ConsigneeMarketServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely used for CRUD operations on market data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **kneCBListSOA**: For managing list-based forms.
2. **kneFRfindCriteria**: For search criteria functionality.
3. **knePrivileges**: For managing user privileges.

### Custom Components:
1. **FRAMEfindCriteriaCodeDesc**: A custom frame for search criteria.

---

## 9. Fields and Validations Listing:

1. **Market Code** (type: string, required, unique).
2. **Description** (type: string, required).
3. **Region Code** (type: string, required).
4. **Region** (type: string, optional).
5. **Status** (type: string, optional).

Mapping of displayed values to database columns:
- `marketCode` → `DBTXTmarketCode`.
- `description` → `DBTXTdescription`.
- `regionCode` → `DBTXTregionCode`.
- `region` → `DBTXTregion1`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [Load Grid Data] --> [User Interaction] --> [Perform Action] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Clicks Button
Form --> Grid: Updates Data
Form --> Service: Sends/Receives Data
```

### Code Snippets:
```delphi
procedure TFORMLconsMarket.GridSetup;
begin
  inherited;
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('stat; marketCode; description; regionCode; region');
    AddCustomField('stat','cxEDTstatus');
  end;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`GridSetup`**: Configures the grid with hidden fields, order fields, and custom fields.
- **`CreateListForm`**: Initializes the form and sets up the grid.

---

## 12. Conclusion:

The `LconsMarket` code unit provides a robust framework for managing and displaying market data in a grid format. While it offers essential functionalities like CRUD operations and search capabilities, the lack of explicit API details and error handling mechanisms could be a limitation.

---

## 13. Short Summary:

The `LconsMarket` unit manages a grid-based display of market data, supporting CRUD operations and search functionalities. It integrates with external libraries for enhanced features and is part of a larger system for managing consignee markets.#### **LconsMarket.pas**

```
unit LconsMarket;

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
  TFORMLconsMarket = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTmarketCode: TsDBText;
    DBTXTdescription: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBTXTregionCode: TsDBText;
    DBTXTregion1: TsDBText;
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
  FORMLconsMarket: TFORMLconsMarket;

implementation

uses
  ConsigneeMarketServiceUtils, MConsMarket;

{$R *.dfm}

{ TFORMLconsMarket }

class function TFORMLconsMarket.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLconsMarket.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLconsMarket.EventSetup;
begin
  inherited;

end;

procedure TFORMLconsMarket.GridSetup;
begin
  inherited;

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields('stat; marketCode; description; regionCode; region');

    AddCustomField('stat','cxEDTstatus');
  end;

end;

class procedure TFORMLconsMarket.Initialize(
  const pv_FormList: TFORMkneCBList);
begin
  inherited;

```

#### **LconsMarket.dfm**

```
inherited FORMLconsMarket: TFORMLconsMarket
  Left = 158
  Top = 186
  Caption = 'Consignee Market List'
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
          Width = 116
          Height = 16
          Caption = 'Consignee Market'
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
        object sLabel1: TsLabel
          Left = 8
          Top = 176
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
          Top = 195
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
```
<!-- tabs:end -->

