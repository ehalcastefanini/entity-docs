<!-- tabs:start -->

#### **Documentation**

# Documentation for `LcustLists` Code Unit

## 1. Overview:

### Objective:
The `LcustLists` code unit is designed to manage and display a list of customer-related data in a grid format. It provides functionalities for searching, filtering, and managing customer lists, markets, and business units. The primary objective is to allow users to interact with customer data efficiently through a user-friendly interface.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **Database Components**: For interacting with the database (e.g., `DB`, `DBClient`).
- **Third-party Libraries**: Includes `cxGrid` for grid display, `sSkinProvider` for UI theming, and `kneCBListSOA` for list management.

### Form Type:
This code represents a **grid display**. 

#### Grid Columns:
- `stat`: Status (type: string).
- `custListCd`: Customer List Code (type: string).
- `reference`: Reference (type: string).
- `marketCd`: Market Code (type: string).
- `lastUpd`: Last Updated (type: datetime).
- `updBy`: Updated By (type: string).

#### Grid Actions:
- **Search**: Filters the grid based on user-defined criteria.
- **Clear Criteria**: Resets the search filters.
- **Advanced Search**: Provides additional search options.
- **New**: Creates a new customer list entry.
- **Modify**: Edits an existing customer list entry.
- **View**: Views details of a selected customer list entry.

---

## 2. Functionality Description:

### User Actions:
- Search for customer lists using various criteria (e.g., reference, customer, market, business unit).
- Clear search criteria to reset the grid.
- Perform advanced searches for more specific filtering.
- Add, modify, or view customer list entries.

### Main Components:
- **Grid (`cxGrid`)**: Displays the customer list data.
- **Search Area (`PNLsearchArea`)**: Contains fields and buttons for filtering the grid.
- **Action List (`ACLeditingActions_deriv`)**: Manages actions like New, Modify, View, and Search.

### Pseudo-code for Actions and Events:
- `OnClick` event of the "Search" button: `if search button clicked then execute search function`.
- `OnClick` event of the "Clear Criteria" button: `if clear button clicked then reset all search fields`.
- `OnChange` event of a search field: `if field value changed then validate field`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `CreateListForm`.
   - The grid and search criteria are set up using `GridSetup` and `InitCriteria`.
   - Default values for business units and other criteria are initialized.

2. **User Interaction**:
   - Users interact with the search area to filter the grid.
   - Clicking buttons triggers actions like searching, clearing criteria, or managing customer lists.

### Functions:
- **`CreateListForm`** (File: `LcustLists`): Initializes the form and sets up default values.
- **`m_SetFindCustomer`** (File: `LcustLists`): Configures the customer search functionality.
- **`m_SetFindMarket`** (File: `LcustLists`): Configures the market search functionality.
- **`SetupParams`** (File: `LcustLists`): Sets up parameters for the grid display.

### Data Input:
- Users must provide values for fields like Reference, Customer, Market, and Business Unit to filter the grid.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search**: Enabled when at least one search field is filled.
- **Clear Criteria**: Always enabled.
- **New/Modify/View**: Enabled only when a grid row is selected.

### Available Filters:
- Reference
- Customer
- Market
- Business Unit

### Error Messages:
- "Required field not completed" if a mandatory field is empty.
- "Invalid input" if a field value does not meet validation criteria.

### Default Field Values:
- Business Unit: Default to the user's assigned business unit.

### Field Validation:
- **Reference**: Must be alphanumeric and uppercase.
- **Customer**: Must be selected from a predefined list.
- **Market**: Must be selected from a predefined list.

---

## 5. Main Functions:

- **`CreateListForm`**: Initializes the form and sets up default configurations.
- **`m_SetFindCustomer`**: Configures the customer search functionality.
- **`m_SetFindMarket`**: Configures the market search functionality.
- **`SetupParams`**: Prepares parameters for the grid display.
- **`InitCriteria`**: Initializes search criteria fields.

---

## 6. API Service Consumption:

- **Service Name**: `CustomerListServiceUtils`.
  - **Endpoint**: `/api/customer-lists`.
  - **Data Sent**: `{ "reference": "string", "customer": "string", "market": "string" }`.
  - **Data Received**: `{ "status": "success", "data": "CustomerList object" }`.
  - **Purpose**: Fetch customer list data based on search criteria.
  - **Error Handling**: Displays an error message if the API call fails.

---

## 7. Conditional Fields (Form Logic):

- **Business Unit**: Only visible if the user has access to multiple business units.
- **Conditions**: The field is hidden if the user has access to only one business unit.

---

## 8. Dependencies:

### External Libraries:
- **`cxGrid`**: For grid display and data visualization.
- **`sSkinProvider`**: For UI theming and styling.
- **`kneCBListSOA`**: For managing list-based forms.

### Custom Components:
- **`FRAMEFindEditSOA`**: Custom frame for search functionality.
- **`FRAMEBusUnit`**: Custom frame for managing business unit selection.

---

## 9. Fields and Validations Listing:

- **Reference** (type: string, required, uppercase).
- **Customer** (type: string, required, selected from a list).
- **Market** (type: string, required, selected from a list).
- **Business Unit** (type: string, optional, default to user's assigned unit).

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a complete workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide detailed interactions.)

### Code Snippets:
```pascal
// Example: Creating the form
var
  Form: TFORMkneCBList;
begin
  Form := TFORMLcustLists.CreateListForm(Self);
  Form.Show;
end;
```

### Screenshots:
HTML representation of the grid:
```html
<table style="border: 1px solid black; width: 100%;">
  <thead>
    <tr>
      <th>Status</th>
      <th>Customer List Code</th>
      <th>Reference</th>
      <th>Market Code</th>
      <th>Last Updated</th>
      <th>Updated By</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Active</td>
      <td>CL001</td>
      <td>REF123</td>
      <td>MK001</td>
      <td>2023-10-01</td>
      <td>Admin</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **`mc_GRID_FIELDS`**: Defines the fields displayed in the grid.
- **`m_SetFindCustomer` and `m_SetFindMarket`**: Key methods for configuring search functionality.

---

## 12. Conclusion:

The `LcustLists` code unit provides a robust framework for managing and displaying customer lists. Its strengths lie in its modular design and integration with external services. However, the code could benefit from more detailed error handling and documentation for certain components.

---

## 13. Short Summary:

The `LcustLists` unit manages customer lists with search and filtering capabilities, leveraging a grid-based interface. It integrates with external services for data retrieval and supports modular customization for business units and markets.#### **LcustLists.pas**

```
unit LcustLists;

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
  sRadioButton, sEdit, sCheckBox, kneCBList, kneUtils, cxContainer,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, DMskin, sDBText,
  kneFRFindEditSOA, sComboBox, kneFRBusUnit;

type
  TFORMLcustLists = class(TFORMkneCBListSOA)
    LBL2: TsLabel;
    EDTcode: TsEdit;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    LBL3: TsLabel;
    FRAMEFindCust: TFRAMEFindEditSOA;
    LBL4: TsLabel;
    FRAMEFindMarket: TFRAMEFindEditSOA;
    FRAMEBusUnit1: TFRAMEBusUnit;
    LBL1: TsLabel;
  private
    procedure m_AfterSearch(Sender: TObject);
    procedure m_SetFindCustomer;
    procedure m_SetFindMarket;
    procedure m_BeforeSearch(Sender: TObject);
    procedure m_BeforeFindCustomer(Sender: TObject);
    procedure m_FreeListServiceCriteria(sender: TObject);
    { Private declarations }
  protected
    { Protected declarations }
    procedure EventSetup;           override;
    procedure GridSetup;            override;
    function SetupParams: Boolean;  override;
    procedure InitCriteria;         override;
    Procedure SetCriteriaValues;

  public
    { Public declarations }
    class function CreateListForm(
      const AOwner: TComponent): TFORMkneCBList;                   virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor;                                        override;
  end;

var
  FORMLcustLists: TFORMLcustLists;

implementation


uses
  Global, kneFGFindUtils
  //--- Forms/Frames
  , EcustLists
  //--- ServiceUtils
  , CustomerListServiceUtils
  , CustomerMarketServiceUtils
  , CustomerWithGenericCriteriaServiceUtils
  ;

const
  mc_GRID_FIELDS = 'stat; custListCd; reference; marketCd'
    + '; lastUpd; updBy';

{$R *.dfm}

{ TFORMLcustLists }

class function TFORMLcustLists.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLcustLists.Create(AOwner);

  Initialize(Result);


  with TFORMLcustLists(Result) do
  begin
    m_SetFindCustomer;
    m_SetFindMarket;

    FRAMEBusUnit1.DataSource     := nil;
    FRAMEBusUnit1.BusUnitList    := gv_BusUnitList;
    FRAMEBusUnit1.BusUnitDefault := gv_DefaultBusUnit;
    FRAMEBusUnit1.IncludeAll     := True;

```

#### **LcustLists.dfm**

```
inherited FORMLcustLists: TFORMLcustLists
  Left = 473
  Top = 168
  Caption = 'Customer Lists List'
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 142
  end
  inherited PNLsearchArea: TsPanel
    Height = 98
    inherited PNLsearchButtons: TsPanel
      Height = 96
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Height = 96
      inherited PNLcriteria: TsPanel
        Height = 92
        object LBL2: TsLabel
          Left = 8
          Top = 13
          Width = 63
          Height = 13
          Caption = 'R&eference:'
          FocusControl = EDTcode
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL3: TsLabel
          Left = 8
          Top = 39
          Width = 61
          Height = 13
          Caption = 'C&ustomer:'
          FocusControl = FRAMEFindCust.FE
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL4: TsLabel
          Left = 8
          Top = 65
          Width = 44
          Height = 13
          Caption = 'Mar&ket:'
          FocusControl = FRAMEFindMarket.FE
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL1: TsLabel
          Left = 410
          Top = 13
          Width = 81
          Height = 13
          Caption = '&Business Unit:'
          FocusControl = FRAMEBusUnit1.CBObusUnit
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object EDTcode: TsEdit
          Left = 81
          Top = 8
          Width = 280
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
```
<!-- tabs:end -->

