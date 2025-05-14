<!-- tabs:start -->

#### **Documentation**

# Documentation for `Lagents` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `Lagents` code unit is designed to manage and display a list of agents in a grid-based interface. It provides functionalities for searching, filtering, and interacting with agent data. The form allows users to view, create, modify, and search for agents based on various criteria such as code, name, market, and agent type. This solves the problem of managing agent data efficiently in a user-friendly interface.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface and handling events.
- **Database Components**: For interacting with the database to fetch and display agent data.
- **Custom Components**: Includes components like `TsLabel`, `TsEdit`, `TsCheckBox`, and `TsComboBox` for enhanced UI/UX.
- **Action List**: For managing user actions like creating, modifying, and searching agents.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - Labels (`TsLabel`): Display static text for field descriptions.
  - Text Fields (`TsEdit`): Input fields for agent code and description.
  - Checkboxes (`TsCheckBox`): For filtering active agents.
  - ComboBox (`TsComboBox`): Dropdown for selecting agent types.
  - Grid (`cxGrid`): Displays the list of agents.
- **Form Actions and Effects**:
  - **Search**: Filters the grid based on the entered criteria.
  - **Clear Criteria**: Resets all search fields to their default values.
  - **Create/Modify/View**: Allows users to create, edit, or view agent details.

---

## 2. Functionality Description:

### User Actions:
- **Search for Agents**: Users can filter agents by code, name, market, and agent type.
- **Clear Search Criteria**: Resets all input fields to default values.
- **Create New Agent**: Opens a form to create a new agent.
- **Modify Agent**: Opens a form to edit the selected agent.
- **View Agent**: Opens a form to view details of the selected agent.

### Main Components:
- **Search Area**: Contains input fields and filters for searching agents.
- **Grid Display**: Shows the list of agents based on the search criteria.
- **Action Buttons**: For creating, modifying, and viewing agents.

### Pseudo-code for Actions and Events:
- **Search Button Click**:  
  `if search button clicked then filter grid based on input criteria`
- **Clear Criteria Button Click**:  
  `if clear button clicked then reset all input fields`
- **Create Button Click**:  
  `if create button clicked then open create agent form`
- **Modify Button Click**:  
  `if modify button clicked and agent selected then open modify agent form`
- **View Button Click**:  
  `if view button clicked and agent selected then open view agent form`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings.
   - The grid is populated with all agents from the database.
2. **User Interaction**:
   - Users can enter search criteria and click the search button to filter the grid.
   - Users can clear the search criteria using the clear button.
   - Users can create, modify, or view agents using the respective buttons.

### Functions:
- **`CreateListForm`** (File: `Lagents.pas`): Creates and initializes the agent list form.
- **`EventSetup`** (File: `Lagents.pas`): Sets up event handlers for the form.
- **`GridSetup`** (File: `Lagents.pas`): Configures the grid display.
- **`BTclearCriteriaClick`** (File: `Lagents.pas`): Clears all search criteria.

### Required Data:
- **Search Criteria**: Code, name, market, agent type, and active status.
- **Agent Data**: Retrieved from the database and displayed in the grid.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search**: Requires at least one search criterion to be entered.
- **Clear Criteria**: No preconditions; resets all fields.
- **Create/Modify/View**: Requires an agent to be selected in the grid.

### Available Filters:
- **Code**: Text input.
- **Name**: Text input.
- **Market**: Dropdown selection.
- **Agent Type**: Dropdown selection.
- **Active Only**: Checkbox.

### Error Messages:
- "No agent selected" if modify or view is clicked without selecting an agent.
- "Invalid input" if search criteria are not valid.

### Default Field Values:
- **Active Only**: Checked by default.
- **Agent Type**: Default to the first item in the dropdown.

### Field Validation:
- **Code**: Must be alphanumeric.
- **Name**: Must not exceed 50 characters.
- **Agent Type**: Must be a valid selection from the dropdown.

---

## 5. Main Functions:

- **`CreateListForm`**: Initializes the form and sets up components.
- **`GridSetup`**: Configures the grid to display agent data.
- **`EventSetup`**: Sets up event handlers for user actions.
- **`BTclearCriteriaClick`**: Clears all search criteria.

---

## 6. API Service Consumption:

- **Service Name**: `AgentWithBusinessUnitServiceUtils`
- **Endpoint**: `/api/agents`
- **Data Sent**: `{ "code": "string", "name": "string", "market": "string", "type": "string", "active": "boolean" }`
- **Data Received**: `{ "status": "success", "data": [Agent objects] }`
- **Purpose**: Fetch agent data based on search criteria.
- **Error Handling**: Displays an error message if the API call fails.

---

## 7. Conditional Fields (Form Logic):

- **Agent Type Dropdown**: Only enabled if the "Active Only" checkbox is checked.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display.
- **TsComponents**: For enhanced UI components.

### Custom Components:
- **`FRAMEFindEditSOA`**: Custom component for advanced search functionality.

---

## 9. Fields and Validations Listing:

- **Code**: (type: string, required, alphanumeric).
- **Name**: (type: string, optional, max: 50 characters).
- **Market**: (type: string, optional).
- **Agent Type**: (type: string, optional, dropdown selection).
- **Active Only**: (type: boolean, default: true).

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as no specific flowchart is provided in the code.)

### Sequence Diagram:
(Not applicable as no specific sequence diagram is provided in the code.)

### Code Snippets:
```pascal
procedure TFORMLagents.BTclearCriteriaClick(Sender: TObject);
begin
  // Clear all search criteria
  EDTcode.Text := '';
  EDTdescription.Text := '';
  CHKactive.Checked := True;
  CBOagentType.ItemIndex := -1;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 906px; padding: 10px; font-family: Tahoma;">
  <label for="code">Code:</label>
  <input type="text" id="code" style="width: 150px;" />
  <label for="name">Name:</label>
  <input type="text" id="name" style="width: 300px;" />
  <label for="market">Market:</label>
  <select id="market" style="width: 150px;">
    <option>Market 1</option>
    <option>Market 2</option>
  </select>
  <label for="active">Active Only:</label>
  <input type="checkbox" id="active" checked />
</div>
```

---

## 11. Important Comments in the Code:

- **`CreateListForm`**: Initializes the form and sets up default configurations.
- **`BTclearCriteriaClick`**: Clears all search criteria and resets fields.

---

## 12. Conclusion:

The `Lagents` code unit provides a robust interface for managing agent data. It is well-structured and integrates seamlessly with the database and external services. However, it could benefit from additional error handling and more detailed field validations.

---

## 13. Short Summary:

The `Lagents` code unit is a Delphi-based form for managing agents, featuring search, filter, and CRUD functionalities. It integrates with external services and provides a user-friendly interface for efficient agent management.#### **Lagents.pas**

```
unit Lagents;

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
  kneEnterAsTab, knePrivileges, sCheckBox, kneFRFindEditSOA, sEdit,
  sComboBox;

type
  TFORMLagents = class(TFORMkneCBListSOA)
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
    DBTXTmarketDesc: TsDBText;
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
    FRAMEfindMarket: TFRAMEFindEditSOA;
    CHKactive: TsCheckBox;
    CBOagentType: TsComboBox;
    sLabel5: TsLabel;
    procedure BTclearCriteriaClick(Sender: TObject);
  private
    procedure m_SetFindMarket;
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
  FORMLagents: TFORMLagents;

implementation

uses
  Global, AgentWithBusinessUnitServiceUtils{NAVOPTECH2022-4802}, Magents,
  CustomerMarketServiceUtils;

{$R *.dfm}

{ TFORMLagents }

class function TFORMLagents.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLagents.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLagents.EventSetup;
begin
  inherited;

end;
```

#### **Lagents.dfm**

```
inherited FORMLagents: TFORMLagents
  Left = 105
  Top = 119
  Caption = 'Agents List'
  ClientHeight = 575
  ClientWidth = 906
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Width = 906
  end
  inherited PNLsearchArea: TsPanel
    Width = 906
    inherited PNLsearchButtons: TsPanel
      Left = 812
    end
    inherited SRBcriteria: TsScrollBox
      Width = 811
      inherited PNLcriteria: TsPanel
        Width = 807
        object LBLname: TsLabel
          Left = 176
          Top = 15
          Width = 31
          Height = 13
          Caption = '&Name:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLCountry: TsLabel
          Left = 176
          Top = 45
          Width = 37
          Height = 13
          Caption = 'Market:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLcode: TsLabel
          Left = 8
          Top = 15
          Width = 29
          Height = 13
          Caption = 'C&ode:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLactive: TsLabel
          Left = 8
          Top = 44
          Width = 59
          Height = 13
          Caption = 'Active Only:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object sLabel5: TsLabel
          Left = 512
          Top = 45
          Width = 60
          Height = 13
          Caption = 'Agent Type:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object EDTdescription: TsEdit
          Left = 216
          Top = 10
          Width = 465
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          SkinData.SkinSection = 'EDIT'
```
<!-- tabs:end -->

