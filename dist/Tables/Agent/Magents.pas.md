<!-- tabs:start -->

#### **Documentation**

# Documentation for `Magents` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `Magents` code unit is designed to manage agents and their associated data, such as addresses, contacts, markets, plafonds, and customers. It provides a user interface for viewing, editing, and managing agent-related information in a structured and organized manner. The main objective is to streamline the management of agent data and their relationships with other entities.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **Object-Oriented Programming (OOP)**: The code uses classes and inheritance to structure functionality.
- **Database Integration**: The code interacts with datasets to manage and display data.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `TsSplitter`: Used to divide sections of the form.
  - `TsPageControl` and `TsTabSheet`: Used for tabbed navigation between different sections (e.g., addresses, markets, plafonds, customers).
  - `TsScrollBox`: A scrollable container for displaying agent details.
  - `TFRAMElistAddresses`, `TFRAMElistContacts`, `TFRAMElistMarkets`, `TFRAMElistPlafonds`, `TFRAMEagentsCustomers`: Custom frames for managing specific data related to agents.
  - `TFRAMEagents`: A custom frame for managing agent details.
- **Form Actions and Effects**:
  - Print the current record.
  - Manage agent-related data (addresses, contacts, markets, plafonds, customers).
  - Validate and retrieve data.

---

## 2. Functionality Description:

### User/Software Actions:
- Print the current record.
- Navigate through tabs to manage different aspects of agent data.
- Add, edit, or delete agent-related data (e.g., addresses, contacts, markets).
- Validate data before saving.

### Main Components:
- **`ACTprint`**: Action for printing the current record.
- **`PGCaddress`**: Page control for navigating between tabs.
- **Custom Frames**: Each frame (e.g., `FRAMElistAddresses1`) is responsible for managing a specific type of data.

### Pseudo-Code for Actions and Events:
- **OnClick Event of Print Button**:  
  `if print button clicked then execute print function`.
- **OnCreate Event of Form**:  
  `if form is created then initialize components and set default values`.
- **OnClick Event of Delete Button in Markets Frame**:  
  `if delete button clicked in markets frame then delete selected market`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created (`FormCreate`), and components are initialized.
   - The first tab (`Addresses`) is set as the active page.
   - Default values are assigned (e.g., `FAgentMarketsList` is initialized to `';'`).

2. **User Interactions**:
   - Users can navigate through tabs to manage different data types.
   - Clicking buttons triggers actions like printing or deleting records.

3. **Functions**:
   - **`FormCreate`** (File: `Magents`): Initializes the form and sets default values.
   - **`m_getData`** (File: `Magents`): Retrieves and binds data to the respective frames.
   - **`ACTprintExecute`** (File: `Magents`): Executes the print action.

### Required Data:
- Users must provide agent details, including addresses, contacts, markets, plafonds, and customers, to fully utilize the form's functionalities.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Print Action**: Enabled when a record is selected.
- **Delete Action in Markets Frame**: Enabled when a market is selected.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- `FAgentMarketsList`: Default value is `';'`.

### Field Validation and Conditions:
- Validation logic is implemented in the `m_Validate` method but is not explicitly detailed in the code.

---

## 5. Main Functions:

- **`m_CreateFormEdit`**: Creates and initializes the form.
- **`FormCreate`**: Sets up the form and initializes default values.
- **`m_getData`**: Retrieves and binds data to the respective frames.
- **`m_Validate`**: Validates the data before saving.
- **`ACTprintExecute`**: Handles the print action.

---

## 6. API Service Consumption:

No external API calls are explicitly defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components**: Used for UI elements.
- **Custom Components**:
  - `TFRAMElistAddresses`, `TFRAMElistContacts`, `TFRAMElistMarkets`, `TFRAMElistPlafonds`, `TFRAMEagentsCustomers`, `TFRAMEagents`: Custom frames for managing specific data.

---

## 9. Fields and Validations Listing:

- **AgentMarketsList**:
  - Type: String
  - Default: `';'`
  - Validation: Not explicitly defined in the code.

Mapping of displayed values and database columns is not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [FormCreate: Initialize Components] --> [User Interaction: Navigate Tabs or Click Buttons] --> [Execute Actions: Print, Delete, etc.] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
User --> Form: Navigate Tabs
User --> Form: Click Print/Delete
Form --> Backend: Execute Action
```

### Code Snippets:
```delphi
procedure TFORMMagents.FormCreate(Sender: TObject);
begin
  inherited;
  PGCaddress.ActivePageIndex := 0;
  FAgentMarketsList := ';';
end;
```

### Screenshots:
Not applicable as the DFM file is not provided.

---

## 11. Important Comments in the Code:

- `FAgentMarketsList`: Initialized to `';'` for optimization purposes.
- `m_getData`: Optimizes resource usage by binding datasets to frames.

---

## 12. Conclusion:

The `Magents` code unit provides a comprehensive interface for managing agent-related data. Its modular design, using custom frames, ensures scalability and maintainability. However, the lack of explicit error handling and validation logic in the provided code may require further implementation.

---

## 13. Short Summary:

The `Magents` code unit is a Delphi-based form for managing agents and their related data, including addresses, contacts, markets, plafonds, and customers. It uses custom frames for modularity and provides actions like printing and data validation.#### **Magents.pas**

```
unit Magents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA, DBClient,
  kneFRCtrlEditSOA, FRagents, knePrivileges, ImgList,
  sSpeedButton, sBitBtn, ToolWin, ComCtrls, acCoolBar, sPanel,
  kneEnterAsTab, sPageControl, ActnList, kneFRGridEditSOA,
  sSplitter, FRlistcontacts, FRlistAddresses, FRlistMarkets, FRlistPlafonds,
  FRagentsCustomers, sScrollBox;

type
  TFORMMagents = class(TFORMkneBaseEdit)
    ACTprint: TAction;
    SPL1: TsSplitter;
    PGCaddress: TsPageControl;
    TSHaddresses: TsTabSheet;
    SPL2: TsSplitter;
    FRAMElistAddresses1: TFRAMElistAddresses;
    FRAMElistContacts1: TFRAMElistContacts;
    TSHmarkets: TsTabSheet;
    FRAMElistMarkets1: TFRAMElistMarkets;
    TSHplafonds: TsTabSheet;
    FRAMElistPlafonds1: TFRAMElistPlafonds;
    TSHcustomers: TsTabSheet;
    FRAMEagentsCustomers1: TFRAMEagentsCustomers;
    SCBagent: TsScrollBox;
    FRAMEagents1: TFRAMEagents;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure ACTprintExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure FRAMElistMarkets1BTNdeleteClick(Sender: TObject);
 
  private
    FAgentMarketsList: string;        //JAR #5930  02-06-2010
    { Private declarations }
    class function m_CreateFormEdit(
      const AOwner: TComponent): TFORMkneBaseEdit;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure SetAgentMarketsList(const Value: string);

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override; 

  public
    { Public declarations }
    procedure m_AllCustMktStateChange;

  published
    { published declarations }
    property AgentMarketsList: string read FAgentMarketsList write SetAgentMarketsList;
  end;

var
  FORMMagents: TFORMMagents;

implementation

{$R *.dfm}

uses
  kneUtils, Global,
  //---
  MaddressAndContact, AgentWithBusinessUnitServiceUtils{NAVOPTECH2022-4802},
  //---
  DRentities;

{ TFORMMagents }

class function TFORMMagents.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
   Result := TFORMMagents.Create(Application);
end;

procedure TFORMMagents.FormCreate(Sender: TObject);
begin
  inherited;
  PGCaddress.ActivePageIndex := 0;
  FAgentMarketsList := ';';          //JAR #5930  02-06-2010
end;


procedure TFORMMagents.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMElistAddresses1.MasterSource := lv_MasterFrame.DStable;        // Detail <- Master
  FRAMElistContacts1.MasterSource  := FRAMElistAddresses1.DStable;   // Detail <- Detail(master)
  FRAMElistMarkets1.MasterSource   := lv_MasterFrame.DStable;        // Detail <- Master
  FRAMElistPlafonds1.MasterSource  := lv_MasterFrame.DStable;        // Detail <- Master
  FRAMEagentsCustomers1.MasterSource  := lv_MasterFrame.DStable;     // Detail <- Master
```

#### **Magents.dfm**

```
inherited FORMMagents: TFORMMagents
  Left = 185
  Top = 101
  Width = 1095
  Height = 671
  Caption = 'Agents Management'
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SPL1: TsSplitter [1]
    Left = 0
    Top = 267
    Width = 1079
    Height = 4
    Cursor = crVSplit
    Align = alTop
    SkinData.SkinSection = 'FORM'
  end
  object SCBagent: TsScrollBox [2]
    Left = 0
    Top = 41
    Width = 1079
    Height = 226
    Align = alTop
    TabOrder = 2
    SkinData.SkinSection = 'PANEL_LOW'
    inline FRAMEagents1: TFRAMEagents
      Left = 0
      Top = 0
      Width = 1075
      Height = 222
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBL1: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindMarket.DBE
      end
      inherited LBLcountry: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindCountry.DBE
      end
      inherited LBLlanguage: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindLanguage.DBE
      end
      inherited LBLLegalNum: TsLabel
        Font.Color = 5059883
      end
      inherited LBLmarket: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindMarket.DBE
      end
      inherited sLabel2: TsLabel
        Font.Color = 5059883
      end
      inherited sLabel5: TsLabel
        Font.Color = 5059883
      end
      inherited sLabel4: TsLabel
        Font.Color = 5059883
      end
      inherited LBLcurrency: TsLabel
        FocusControl = FRAMEagents1.FRAMEfindCurrency.DBE
        Font.Color = 5059883
      end
      inherited Label1: TsLabel
        Font.Color = 5059883
      end
      inherited PNLfooter: TsPanel
        Top = 188
        Width = 1075
      end
      inherited CBOcommMthd: TcxDBImageComboBox
        TabOrder = 18
        Width = 85
      end
      inherited CHKallCustMkt: TcxDBCheckBox
        Width = 241
      end
      inherited FRAMEfindCountry: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            DataSource = FRAMEagents1.DStable
          end
        end
        inherited PNLcode: TPanel
          inherited DBE: TsDBEdit
            DataSource = FRAMEagents1.DStable
          end
        end
      end
      inherited FRAMEfindCurrency: TFRAMEFindEditSOA
        inherited PNLdesc: TPanel
          inherited DBEDesc: TsDBEdit
            DataSource = FRAMEagents1.DStable
          end
```
<!-- tabs:end -->

