<!-- tabs:start -->

#### **Documentation**

# Documentation for `LdocsCheckListRules`

## 1. Overview:

### Objective:
The `LdocsCheckListRules` unit is designed to manage and display a checklist of document rules in a grid format. It provides functionalities for searching, filtering, and managing document rules based on various criteria such as customer, market, region, and more. The main objective is to streamline the process of managing document rules by providing a user-friendly interface with advanced search and filtering capabilities.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and handling events.
- **Third-party Libraries**: Includes components like `TsCheckBox`, `TsLabel`, `TsPanel`, and `cxGrid` for enhanced UI and grid functionalities.
- **Database Connectivity**: Uses `DB` and `DBClient` for database interactions.
- **Custom Components**: Includes custom components like `kneCBListSOA`, `kneFRFindEditSOA`, and `kneFRBusUnit`.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
- `stat`: Status (e.g., active/inactive).
- `ruleId`: Rule ID (integer or string).
- `ruleDesc`: Rule Description (string).
- `businessUnit`: Business Unit (string).
- `lastUpd`: Last Updated Date (date/time).
- `updBy`: Updated By (string).

#### Grid Actions and Their Effects:
- **New**: Adds a new document rule.
- **Modify**: Edits an existing document rule.
- **View**: Views details of a document rule.
- **Search**: Filters the grid based on search criteria.
- **Advanced Search**: Provides additional filtering options.

---

## 2. Functionality Description:

### User Actions:
- Add, modify, or view document rules.
- Search and filter document rules based on various criteria.
- Enable or disable document rules using a checkbox.

### Main Components:
- **Grid (`cxGrid`)**: Displays the list of document rules.
- **Search Area (`PNLsearchArea`)**: Contains search fields and buttons.
- **Criteria Fields (`FRAMEFindEditSOA`)**: Custom components for selecting search criteria like customer, market, region, etc.
- **Action Buttons**: Buttons for performing actions like search, clear criteria, and manage rules.

### Pseudo-code for Actions and Events:
- **OnClick event of "Search" button**:  
  `if search button clicked then execute search function with criteria`.
- **OnClick event of "Clear Criteria" button**:  
  `if clear criteria button clicked then reset all search fields`.
- **OnClick event of "New" button**:  
  `if new button clicked then open form to create a new rule`.
- **OnClick event of "Modify" button**:  
  `if modify button clicked then open form to edit the selected rule`.
- **OnClick event of "View" button**:  
  `if view button clicked then open form to view details of the selected rule`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: The form initializes and loads the grid with document rules from the database.
2. **User Interaction**: Users can interact with the grid and search area to filter or manage rules.
3. **Event Handling**: Button clicks trigger specific actions like searching, clearing criteria, or managing rules.

### Functions:
- **`m_SetFindCustomer`**: Sets up the customer search criteria.
- **`m_SetFindCustMarket`**: Sets up the customer market search criteria.
- **`m_SetFindConsignee`**: Sets up the consignee search criteria.
- **`m_BeforeSearch`**: Prepares the search criteria before executing the search.
- **`CreateEditor`**: Creates an editor for managing rules.

### Data Input:
- Users must provide search criteria such as customer, market, region, etc., to filter the grid.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search**: Requires at least one search criterion to be filled.
- **Modify/View**: Requires a rule to be selected in the grid.
- **New**: No preconditions.

### Available Filters:
- Customer
- Customer Market
- Consignee
- Consignee Market
- Region
- Incoterm
- Payment
- Document Type
- Mill

### Error Messages:
- "No criteria provided" if the search is executed without any criteria.
- "No rule selected" if modify or view is clicked without selecting a rule.

### Default Field Values:
- Checkbox "Active": Default is checked (active).

### Field Validation and Conditions:
- All search fields must be validated to ensure they are not empty when required.
- The "Active" checkbox toggles the inclusion of inactive rules in the grid.

---

## 5. Main Functions:

- **`CreateListForm`**: Creates and initializes the form.
- **`Initialize`**: Sets up the form with default values and configurations.
- **`CreateEditor`**: Opens the editor for managing rules.
- **`SetupParams`**: Configures parameters for the grid and search area.
- **`SetCriteriaValues`**: Applies the search criteria to the grid.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `CheckListDocRulesServiceUtils`
   - **Endpoint**: `/api/checklist-doc-rules`
   - **Data Sent**: `{ "criteria": { "customer": "string", "region": "string" } }`
   - **Data Received**: `{ "status": "success", "data": [ { "ruleId": "int", "ruleDesc": "string" } ] }`
   - **Purpose**: Fetches document rules based on search criteria.
   - **Error Handling**: Displays "Failed to fetch data" if the call fails.

---

## 7. Conditional Fields (Form Logic):

- The "Active" checkbox determines whether inactive rules are included in the grid.
- Conditions: If unchecked, only active rules are displayed.

---

## 8. Dependencies:

### External Libraries:
- **`cxGrid`**: For grid display and management.
- **`TsCheckBox`, `TsLabel`, `TsPanel`**: For UI components.

### Custom Components:
- **`kneCBListSOA`**: Base class for the form.
- **`kneFRFindEditSOA`**: Custom search field components.
- **`kneFRBusUnit`**: Component for selecting business units.

---

## 9. Fields and Validations Listing:

- **Customer**: Type: string, required.
- **Customer Market**: Type: string, optional.
- **Consignee**: Type: string, optional.
- **Consignee Market**: Type: string, optional.
- **Region**: Type: string, optional.
- **Incoterm**: Type: string, optional.
- **Payment**: Type: string, optional.
- **Document Type**: Type: string, optional.
- **Mill**: Type: string, optional.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as no specific flowchart is provided in the code.)

### Sequence Diagram:
(Not applicable as no specific sequence diagram is provided in the code.)

### Code Snippets:
```pascal
procedure TFORMLdocsCheckListRules.m_BeforeSearch(Sender: TObject);
begin
  // Prepare search criteria
  SetCriteriaValues;
end;
```

### Screenshots:
(Not applicable as the DFM file is not fully provided.)

---

## 11. Important Comments in the Code:

- **`mc_GRID_FIELDS`**: Defines the fields displayed in the grid.
- **`m_BeforeSearch`**: Critical for preparing search criteria before executing the search.

---

## 12. Conclusion:

The `LdocsCheckListRules` unit provides a robust interface for managing document rules with advanced search and filtering capabilities. While it is feature-rich, the lack of detailed error handling and validation logic in the code could be a limitation.

---

## 13. Short Summary:

The `LdocsCheckListRules` unit manages document rules in a grid format, offering search, filter, and management functionalities. It integrates with external services for data retrieval and uses custom components for enhanced UI and functionality.#### **LdocsCheckListRules.pas**

```
unit LdocsCheckListRules;

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
  sCheckBox, kneCBList, kneFRFindEditSOA, kneFRBusUnit
  ;

type
  TFORMLdocsCheckListRules = class(TFORMkneCBListSOA)
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    CHKactive: TsCheckBox;
    FRAMEFindCust: TFRAMEFindEditSOA;
    FRAMEFindCustMkt: TFRAMEFindEditSOA;
    FRAMEFindCons: TFRAMEFindEditSOA;
    FRAMEFindConsMkt: TFRAMEFindEditSOA;
    FRAMEFindRegion: TFRAMEFindEditSOA;
    FRAMEFindIncoterm: TFRAMEFindEditSOA;
    LBL1: TsLabel;
    LBL2: TsLabel;
    LBL3: TsLabel;
    LBL4: TsLabel;
    LBL5: TsLabel;
    LBL6: TsLabel;
    LBL7: TsLabel;
    LBL8: TsLabel;
    FRAMEFindPayment: TFRAMEFindEditSOA;
    FRAMEFindDocs: TFRAMEFindEditSOA;
    FRAMEBusUnit1: TFRAMEBusUnit;
    LBL9: TsLabel;
    sLabel1: TsLabel;
    FRAMEFindMill: TFRAMEFindEditSOA;
  private
    procedure m_SetFindCustomer;
    procedure m_SetFindCustMarket;
    procedure m_SetFindConsignee;
    procedure m_SetFindConsMarket;
    procedure m_SetFindRegion;
    procedure m_SetFindIncoterm;
    procedure m_SetFindPayment;
    procedure m_SetFindDocTp;
    procedure m_BeforeSearch(Sender: TObject);
    procedure m_SetFindMill;
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
  FORMLdocsCheckListRules: TFORMLdocsCheckListRules;

implementation


uses
  Global, kneFGFindUtils
  //--- Forms/Frames
  , EdocsCheckListRules
  //--- ServiceUtils
  , CheckListDocRulesServiceUtils
  , CustomerMarketServiceUtils
  , ConsigneeMarketServiceUtils
  , RegionServiceUtils
  , DeliveryTermServiceUtils
  , PaymentCustomerServiceUtils
  , CheckListDocsToAddServiceUtils
  , MillServiceUtils {#24067}
  ;

const
  mc_GRID_FIELDS = 'stat; ruleId; ruleDesc; businessUnit; lastUpd; updBy';

{$R *.dfm}

{ TFORMLdocsCheckListRules }
```

#### **LdocsCheckListRules.dfm**

```
inherited FORMLdocsCheckListRules: TFORMLdocsCheckListRules
  Left = 473
  Top = 168
  Caption = 'Documents Check List Rules'
  ClientWidth = 978
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 267
    Width = 978
  end
  inherited PNLsearchArea: TsPanel
    Width = 978
    Height = 223
    inherited PNLsearchButtons: TsPanel
      Left = 884
      Height = 221
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      Width = 883
      Height = 221
      inherited PNLcriteria: TsPanel
        Width = 879
        Height = 217
        object LBL1: TsLabel
          Left = 8
          Top = 13
          Width = 61
          Height = 13
          Caption = 'Customer:'
          FocusControl = FRAMEFindCust
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL2: TsLabel
          Left = 8
          Top = 39
          Width = 78
          Height = 13
          Caption = 'Cust. Market:'
          FocusControl = FRAMEFindCustMkt
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL3: TsLabel
          Left = 8
          Top = 65
          Width = 65
          Height = 13
          Caption = 'Consignee:'
          FocusControl = FRAMEFindCons
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL4: TsLabel
          Left = 8
          Top = 91
          Width = 81
          Height = 13
          Caption = 'Cons. Market:'
          FocusControl = FRAMEFindConsMkt
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
          Font.Style = []
        end
        object LBL5: TsLabel
          Left = 8
          Top = 117
          Width = 78
          Height = 13
          Caption = 'Cust. Region:'
          FocusControl = FRAMEFindRegion
          ParentFont = False
          Font.Charset = ANSI_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Verdana'
```
<!-- tabs:end -->

