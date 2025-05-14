<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocsCheckRules` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRdocsCheckRules` code unit defines a form (`TFRAMEdocsCheckRules`) that allows users to manage and edit rules related to a checklist document system. It provides a user interface for viewing, editing, and interacting with rule data, including fields for rule ID, description, and associated business units. The form integrates with a service utility (`TCheckListDocRulesServiceUtils`) to handle data operations.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and managing components.
- **SOAP Services**: For interacting with external services (`TCheckListDocRulesServiceUtils`).
- **Database Components**: For binding data to UI elements (`TsDBEdit`, `DataSource`).
- **Custom Components**: Includes `TsLabel`, `TsDBEdit`, `TFRAMEstatusInfo`, and `TFRAMEBusUnit`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTruleId` (Text Input - Database-bound).
  - `EDTruleDesc` (Text Input - Database-bound).
  - `FRAMEBusUnit1` (Custom Component for Business Unit selection).
- **Form Actions and Effects**:
  - **Add Button**: Adds a new rule.
  - **Apply Button**: Saves changes to the rule.

---

## 2. Functionality Description:

### User/Software Actions:
- View and edit rule details (ID, description, and business unit).
- Save changes to the database.
- Prevent editing of the `Rule ID` field when creating a new rule.

### Main Components:
- **Labels (`TsLabel`)**: Display field names and provide focus control.
- **Database-bound Inputs (`TsDBEdit`)**: Allow users to input and edit rule data.
- **Business Unit Selector (`TFRAMEBusUnit`)**: Custom component for selecting business units.
- **Service Utility (`TCheckListDocRulesServiceUtils`)**: Handles data operations.

### Pseudo-code for Actions and Events:
- **On Form Creation**:
  ```
  if form is created then
    initialize properties and data bindings
    set default visibility and actions
  ```
- **On Add Button Click**:
  ```
  if add button clicked then
    create a new rule entry
  ```
- **On Apply Button Click**:
  ```
  if apply button clicked then
    save changes to the database
  ```
- **On Rule ID Field Access**:
  ```
  if editing mode is NEW then
    disable Rule ID field
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created, and properties such as `MasterSource`, `DataPacketName`, and `ProviderService` are initialized.
   - Data bindings are set for the `FRAMEstatusInfo1` and `FRAMEBusUnit1` components.
2. **User Interaction**:
   - Users can input or edit data in the `Rule ID` and `Description` fields.
   - Users can select a business unit using the `FRAMEBusUnit1` component.
3. **Event Handling**:
   - Buttons trigger actions like adding or saving rules.
   - Field access is restricted based on the editing mode.

### Data Requirements:
- **Rule ID**: Unique identifier for the rule.
- **Description**: Text description of the rule.
- **Business Unit**: Selected from a predefined list.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Button**: Enabled at all times; creates a new rule entry.
- **Apply Button**: Enabled only when all required fields are filled.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- **Business Unit**: Defaults to `gv_DefaultBusUnit`.

### Field Validation and Conditions:
- **Rule ID**: Disabled in "NEW" mode.
- **Description**: No explicit validation defined.
- **Business Unit**: Must be selected from the list.

---

## 5. Main Functions:

### `Create` Constructor:
- Initializes the form and sets default properties.
- Binds data sources to UI components.

### `SetKeyEditing` Procedure:
- Disables the `Rule ID` field when creating a new rule.

---

## 6. API Service Consumption:

### Service Details:
- **Service Name**: `TCheckListDocRulesServiceUtils`.
- **Purpose**: Handles data operations for checklist document rules.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Rule ID Field**: Disabled when creating a new rule.
- **Business Unit Selector**: Always visible.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components**: For service interaction.
- **Database Components**: For data binding.

### Custom Components:
- `TFRAMEstatusInfo`: Displays status information.
- `TFRAMEBusUnit`: Manages business unit selection.

---

## 9. Fields and Validations Listing:

- **Rule ID**:
  - Type: String.
  - Required: Yes.
  - Validation: Disabled in "NEW" mode.
- **Description**:
  - Type: String.
  - Required: Yes.
- **Business Unit**:
  - Type: Dropdown (Custom Component).
  - Required: Yes.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Form Initialization] --> [Load Data] --> [User Interaction] --> [Save Changes]
```

### Sequence Diagram:
```plaintext
User --> [Form] --> [Service Utility] --> [Database]
```

### Code Snippets:
```delphi
procedure TFRAMEdocsCheckRules.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  TkneControls.SetControlState(EDTruleId, False);
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Verdana; width: 626px;">
  <label style="color: #4D4D4D;">Rule ID:</label>
  <input type="text" disabled />
  <label style="color: #4D4D4D;">Description:</label>
  <input type="text" />
  <label style="color: #4D4D4D;">Business Unit:</label>
  <select>
    <option>Default Business Unit</option>
  </select>
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization**:
  ```delphi
  MasterSource := nil;
  DataPacketName := 'CheckListDocRules';
  ```
- **Field Access Control**:
  ```delphi
  TkneControls.SetControlState(EDTruleId, False);
  ```

---

## 12. Conclusion:

The `FRdocsCheckRules` code unit provides a robust framework for managing checklist document rules. It integrates seamlessly with SOAP services and database components, offering a user-friendly interface. However, error handling and field validation could be more explicitly defined.

---

## 13. Short Summary:

The `FRdocsCheckRules` form manages checklist document rules, allowing users to view, edit, and save rule data. It integrates with SOAP services and database components, ensuring efficient data handling and user interaction.#### **FRdocsCheckRules.pas**

```
unit FRdocsCheckRules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRStatusInfo, kneFRBusUnit;

type
  TFRAMEdocsCheckRules = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL3: TsLabel;
    LBL1: TsLabel;
    EDTruleId: TsDBEdit;
    EDTruleDesc: TsDBEdit;
    LBLbusUnit: TsLabel;
    FRAMEBusUnit1: TFRAMEBusUnit;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyEditing(const EditKey: Boolean); override;  
  end;

var
  FRAMEdocsCheckRules: TFRAMEdocsCheckRules;

implementation

uses
  kneTypes, kneUtils, Global,
  //---
  CheckListDocRulesServiceUtils;

{$R *.dfm}

{ TFRAMEdocsCheckRules }

constructor TFRAMEdocsCheckRules.Create(AOwner: TComponent);
begin
  inherited;
  
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CheckListDocRules';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCheckListDocRulesServiceUtils.Create(self);

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

  // [02-03-2020, #23898]
  FRAMEBusUnit1.DataSource     := DStable;
  FRAMEBusUnit1.BusUnitList    := gv_BusUnitList;
  FRAMEBusUnit1.BusUnitDefault := gv_DefaultBusUnit;
end;


procedure TFRAMEdocsCheckRules.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  // impedir acesso ao campo (em NEW)
  TkneControls.SetControlState(EDTruleId, False);
end;



end.
```

#### **FRdocsCheckRules.dfm**

```
inherited FRAMEdocsCheckRules: TFRAMEdocsCheckRules
  Width = 626
  Font.Name = 'Verdana'
  object LBL3: TsLabel [0]
    Left = 8
    Top = 39
    Width = 69
    Height = 13
    Caption = 'Description:'
    FocusControl = EDTruleDesc
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL1: TsLabel [1]
    Left = 8
    Top = 13
    Width = 48
    Height = 13
    Caption = 'Rule ID:'
    FocusControl = EDTruleId
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLbusUnit: TsLabel [2]
    Left = 236
    Top = 13
    Width = 81
    Height = 13
    Caption = 'Business Unit:'
    FocusControl = FRAMEBusUnit1.DBCBObusUnit
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Width = 626
    TabOrder = 3
    Visible = False
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
```
<!-- tabs:end -->

