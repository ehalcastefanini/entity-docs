<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustAgent` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRcustAgent` code unit defines a Delphi frame (`TFRAMEcustAgent`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). It is designed to manage and display customer-agent relationships in a grid format. The frame provides functionalities such as editing, searching, and managing customer-agent data, including commission rates, agent codes, and statuses.

This frame is part of a larger system, likely a CRM or ERP application, where users can interact with customer-agent data in a structured and user-friendly manner.

### Technologies Used:
- **Delphi VCL Framework**: For UI components and event handling.
- **SOAP Services**: For communication with external services (e.g., `AgentServiceUtils`).
- **Database Components**: For data binding and manipulation (`TcxGridDBTableView`, `TClientDataSet`).
- **Custom Components**: Includes custom editors like `cxEDTfind` and `cxICBcommType`.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **mill**: String (Hidden by default).
2. **agentCode**: String (Custom editor: `cxEDTfind`).
3. **agentName**: String.
4. **commRate**: Numeric.
5. **commType**: String (Custom editor: `cxICBcommType`).
6. **stat**: String (Custom editor: `cxEDTstat`).
7. **lastUpd**: DateTime.
8. **updBy**: String.

#### Grid Actions and Their Effects:
- **Edit Value Changed**: Updates the dataset when a value in the grid is modified.
- **Find Agent**: Opens a dialog to search for agents by code or name.
- **Apply Changes**: Saves changes made in the grid.
- **Cancel Changes**: Reverts unsaved changes.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Edit Grid Values**: Users can modify values in the grid, triggering updates to the dataset.
2. **Search for Agents**: Users can search for agents using a custom dialog.
3. **Apply Changes**: Save modifications to the database.
4. **Cancel Changes**: Discard unsaved changes.

### Main Components:
- **Grid (`cxGrid`)**: Displays customer-agent data.
- **Custom Editors**: Enhance user interaction with specific fields.
- **Action Buttons**: Provide functionalities like "Apply" and "Cancel."

### Pseudo-code for Actions and Events:
- `OnEditValueChanged` event: `if grid cell value changed then update dataset`.
- `OnButtonClick` event of `cxEDTfind`: `if button clicked then open find dialog`.
- `Apply Button Click`: `if apply button clicked then save changes`.
- `Cancel Button Click`: `if cancel button clicked then revert changes`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created and configured (`Create` constructor).
   - Grid settings are defined, including hidden fields, field order, and custom editors.
   - Action panel visibility and available actions are set.

2. **User Interaction**:
   - Users interact with the grid to edit values or search for agents.
   - Buttons trigger specific actions like saving or canceling changes.

### Functions and File Locations:
- **`Create` Constructor** (File: `FRcustAgent.pas`):
  - Initializes the frame and configures grid settings.
- **`m_SetFindEditAgent`** (File: `FRcustAgent.pas`):
  - Opens a dialog for searching agents.
- **`cxDBVtableEditValueChanged`** (File: `FRcustAgent.pas`):
  - Handles changes in grid cell values.

### Required Data:
- Users must provide valid values for fields like `agentCode`, `commRate`, and `commType`.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Edit Grid Values**: Requires the grid to be loaded with data.
- **Search for Agents**: Requires the `cxEDTfind` editor to be active.
- **Apply Changes**: Enabled only if there are unsaved changes.
- **Cancel Changes**: Enabled only if there are unsaved changes.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Invalid value" if a grid cell value does not meet validation criteria.
- "Search failed" if no matching agent is found.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **agentCode**: Must be a valid agent code (validated via `m_SetFindEditAgent`).
- **commRate**: Should be a numeric value.
- **commType**: Must match predefined options in `cxICBcommType`.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame and configures grid settings.
2. **`m_SetFindEditAgent`**:
   - Opens a dialog for searching agents.
3. **`cxDBVtableEditValueChanged`**:
   - Updates the dataset when a grid cell value changes.

---

## 6. API Service Consumption:

- **Service Name**: AgentServiceUtils.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Likely includes agent-related data (e.g., `agentCode`, `commRate`).
- **Data Received**: Agent details or confirmation of changes.
- **Purpose**: Manage customer-agent relationships.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Custom Editors**:
  - `agentCode`: Uses `cxEDTfind` for searching agents.
  - `commType`: Uses `cxICBcommType` for selecting commission types.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based communication.
- **cxGrid**: For grid display and interaction.
- **kneUtils**: Utility functions.
- **kneFindDialog**: Custom dialog for searching.

### Custom Components:
- **cxEDTfind**: Custom editor for searching agents.
- **cxICBcommType**: Custom editor for selecting commission types.

---

## 9. Fields and Validations Listing:

1. **mill**: String, hidden.
2. **agentCode**: String, required, custom editor (`cxEDTfind`).
3. **agentName**: String, required.
4. **commRate**: Numeric, required.
5. **commType**: String, required, custom editor (`cxICBcommType`).
6. **stat**: String, optional, custom editor (`cxEDTstat`).
7. **lastUpd**: DateTime, read-only.
8. **updBy**: String, read-only.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEcustAgent.m_SetFindEditAgent(Sender: TObject; AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin
  lv_Find := TkneDialogFactory.GetFindDialog(Application);
  with lv_Find.Options.DataSelection do
  begin
    FieldNameForCode := 'Code';
    FieldNamesForDesc.Add('name');
  end;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Hidden fields: `'HIDE_ALL_FIELDS'`.
  - Field order: `'mill; agentCode; agentName; commRate; commType; stat; lastUpd; updBy'`.
- **Custom Editors**:
  - `agentCode`: `cxEDTfind`.
  - `commType`: `cxICBcommType`.

---

## 12. Conclusion:

The `FRcustAgent` code unit provides a robust framework for managing customer-agent relationships in a grid format. Its strengths include customizable grid settings and integration with external services. However, the lack of explicit error handling and API endpoint definitions are limitations.

---

## 13. Short Summary:

The `FRcustAgent` unit defines a grid-based frame for managing customer-agent relationships, offering functionalities like editing, searching, and saving data. It integrates with SOAP services and custom components for enhanced user interaction.#### **FRcustAgent.pas**

```
unit FRcustAgent;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEcustAgent = class(TFRAMEBaseGridEditSOA)
    cxICBcommType: TcxEditRepositoryImageComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    { Private declarations }
    procedure m_SetFindEditAgent(Sender: TObject;
      AButtonIndex: Integer);
    procedure m_FindByCodeAgent(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure SetKeyEditing(const EditKey: Boolean);  override;
    function SetFieldValuesToCDS(const pv_dataset: TDataset; 
      const pv_fieldsValues: string; pv_Separador: String = '|'): string;  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetForDOCADDR;
  end;

var
  FRAMEcustAgent: TFRAMEcustAgent;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes, kneConfigObjects, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, Global,
  AgentServiceUtils;

{ TFRAMEcustAgent }

constructor TFRAMEcustAgent.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=customer';
  DataPacketName := 'CustomerAgent';
  PropertyName := 'agents';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;
    
  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; agentCode; agentName; commRate; commType; stat; lastUpd; updBy');
    // Key Fields ..............................................................
    KeyFields:= 'customer;mill;agentCode';
    // Custom Editors ..........................................................
    AddCustomField('agentCode','cxEDTfind');
    AddCustomField('commType','cxICBcommType');
    AddCustomField('stat','cxEDTstat');
  end; //with

  ColsWidthInGrid := '45;70;200;55;55;95;105;70';
  cxEDTfind.Properties.OnButtonClick := m_SetFindEditAgent;
end;


procedure TFRAMEcustAgent.m_SetFindEditAgent(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try    // inicializa��o de Find Dialog
    lv_Find := nil;
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find.Options.DataSelection do
    begin
      // campos para selec��o do Find DataSet
      FieldNameForCode:= 'Code';
      FieldNamesForDesc.Clear;
      FieldNamesForDesc.Add('name');
```

#### **FRcustAgent.dfm**

```
inherited FRAMEcustAgent: TFRAMEcustAgent
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
```
<!-- tabs:end -->

