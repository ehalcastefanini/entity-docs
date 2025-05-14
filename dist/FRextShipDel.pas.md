<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRextShipDel` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRextShipDel` code unit defines a form (`TFRAMEextShipDel`) that manages pre-delivery information for shipments. It allows users to configure and manage alert settings such as alert days, alert time, and CSA (Customer Service Agent) emails. The form dynamically enables or disables related fields based on the user's selection of a checkbox (`CHKcheckPreDeliv`).

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **SOAP Services**: Used for data communication (via `SOAPHTTPClient`).
- **Database Components**: Includes `DB`, `DBClient`, and `DBCtrls` for database interaction.
- **Third-party Libraries**: Includes `cxControls`, `cxEdit`, and `TsComponents` for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `CHKcheckPreDeliv` (Checkbox): Toggles pre-delivery information.
  - `EDTcsaEmails` (Text Input): Input for CSA emails.
  - `EDTalertTime` (Time Input): Input for alert time.
  - `EDTalertDays` (Masked Input): Input for alert days.
- **Form Actions and Effects**:
  - Toggling the checkbox enables or disables the related fields.
  - Clearing the checkbox clears the values of the related fields.

---

## 2. Functionality Description:

### User/Software Actions:
- Enable or disable pre-delivery information using the checkbox.
- Input alert days, alert time, and CSA emails when pre-delivery is enabled.
- Automatically clear related fields when pre-delivery is disabled.

### Main Components:
- **Checkbox (`CHKcheckPreDeliv`)**: Controls the state of related fields.
- **Text Input (`EDTcsaEmails`)**: Accepts CSA email addresses.
- **Time Input (`EDTalertTime`)**: Accepts alert time.
- **Masked Input (`EDTalertDays`)**: Accepts alert days.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHKcheckPreDeliv`:
  ```pseudo
  if checkbox is checked then
      enable related fields
  else
      disable related fields and clear their values
  ```
- `AfterEdit` event of the dataset:
  ```pseudo
  if dataset is edited then
      validate and save changes
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings in the `Create` constructor.
   - The `ShowActionPanel` is set to `False`, and no actions are available by default.
   - The `ShowData` method is called to set the initial state of components based on the checkbox value.

2. **User Interaction**:
   - When the checkbox is toggled, the `CHKcheckPreDelivClick` event is triggered, enabling or disabling related fields.
   - If the checkbox is unchecked, the related fields are cleared.

### Functions:
- **`Create` (File: `FRextShipDel.pas`)**:
  Initializes the form and sets default properties.
- **`ShowData` (File: `FRextShipDel.pas`)**:
  Updates the state of components based on the checkbox value.
- **`SetComponentesState` (File: `FRextShipDel.pas`)**:
  Enables or disables related fields.
- **`CHKcheckPreDelivClick` (File: `FRextShipDel.pas`)**:
  Handles the checkbox click event and updates the state of related fields.

### Required Data:
- Checkbox value (`CHKcheckPreDeliv`).
- Alert days (`EDTalertDays`).
- Alert time (`EDTalertTime`).
- CSA emails (`EDTcsaEmails`).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Enable/Disable Fields**:
  - Action: Toggling the checkbox enables or disables related fields.
  - Preconditions: None.
- **Clear Fields**:
  - Action: Clearing the checkbox clears the values of related fields.
  - Preconditions: Checkbox must be unchecked.

### Available Filters:
No filters are explicitly defined in the code.

### Error Messages:
No error messages are explicitly defined in the code.

### Default Field Values:
- Checkbox (`CHKcheckPreDeliv`): Default is unchecked.
- Other fields: No default values are explicitly defined.

### Field Validation and Conditions:
- `EDTcsaEmails`: No validation is explicitly defined.
- `EDTalertTime`: No validation is explicitly defined.
- `EDTalertDays`: No validation is explicitly defined.

---

## 5. Main Functions:

- **`Create`**: Initializes the form and sets default properties.
- **`ShowData`**: Updates the state of components based on the checkbox value.
- **`SetComponentesState`**: Enables or disables related fields.
- **`CHKcheckPreDelivClick`**: Handles the checkbox click event and updates the state of related fields.

---

## 6. API Service Consumption:

No external API calls are explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Conditional Field**: `EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`.
- **Condition**: These fields are visible and editable only when the checkbox (`CHKcheckPreDeliv`) is checked.

---

## 8. Dependencies:

### External Libraries:
- **`cxControls`, `cxEdit`**: Used for enhanced UI components.
- **`TsComponents`**: Used for styled components like labels and checkboxes.

### Custom Components:
- **`TkneControls`**: Used for setting the state of controls.

---

## 9. Fields and Validations Listing:

- **CHKcheckPreDeliv** (Checkbox, required, values: `Y` or `N`).
- **EDTcsaEmails** (Text, optional, no validation defined).
- **EDTalertTime** (Time, optional, no validation defined).
- **EDTalertDays** (Masked, optional, no validation defined).

Mapping of displayed values and database columns:
- `CHKcheckPreDeliv` → `checkPreDeliv`.
- `EDTcsaEmails` → `csaEmails`.
- `EDTalertTime` → `alertTime`.
- `EDTalertDays` → `alertDays`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [ShowData] --> [User Toggles Checkbox]
    --> [Enable/Disable Fields] --> [Clear Fields if Disabled] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Toggles Checkbox
Form --> Dataset: Updates Field States
Dataset --> Form: Saves Changes
```

### Code Snippets:
```delphi
procedure TFRAMEextShipDel.CHKcheckPreDelivClick(Sender: TObject);
begin
  SetComponentesState(CHKcheckPreDeliv.Checked);
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Initialization**:
  ```delphi
  MasterKeyFields := 'marketCode=consMkt';
  DataPacketName  := 'ShipDelConsMkt';
  PropertyName    := 'shipDelConsMkt';
  FrameType       := frtDetail;
  ```

- **Checkbox Click Event**:
  ```delphi
  if not lv_EnableComps then
  begin
    CDStable.FieldByName('alertDays').Clear;
    CDStable.FieldByName('alertTime').Clear;
    CDStable.FieldByName('csaEmails').Clear;
  end;
  ```

---

## 12. Conclusion:

The `FRextShipDel` code unit provides a form for managing pre-delivery information. It dynamically enables or disables fields based on user input, ensuring a clean and intuitive user experience. However, the code lacks explicit error handling and field validation, which could be improved.

---

## 13. Short Summary:

The `FRextShipDel` form manages pre-delivery information by enabling or disabling fields based on a checkbox. It dynamically updates field states and clears values when disabled, ensuring a user-friendly interface for shipment alert configuration.#### **FRextShipDel.pas**

```
unit FRextShipDel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  cxGraphics, cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxImageComboBox, cxDBEdit, kneFRStatusInfo,
  sFrameAdapter, sBitBtn, sPanel, sLabel, sBevel, sCheckBox, sDBCheckBox,
  sDBEdit, cxSpinEdit, cxTimeEdit;

type
  TFRAMEextShipDel = class(TFRAMEBaseCtrlEditSOA)
    LBLname: TsLabel;
    Label1: TsLabel;
    Label2: TsLabel;
    CHKcheckPreDeliv: TsDBCheckBox;
    EDTcsaEmails: TsDBEdit;
    EDTalertTime: TcxDBTimeEdit;
    EDTalertDays: TcxDBMaskEdit;
    procedure CHKcheckPreDelivClick(Sender: TObject);
    procedure CDStableAfterEdit(DataSet: TDataSet);
  private
    procedure SetComponentesState(pv_State: Boolean);
    { Private declarations }

  protected
    function m_Validate: Boolean;  override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ShowData; override;
  end;

var
  FRAMEextShipDel: TFRAMEextShipDel;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDel }

constructor TFRAMEextShipDel.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'marketCode=consMkt';
  DataPacketName  := 'ShipDelConsMkt';
  PropertyName    := 'shipDelConsMkt';
  FrameType       := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  EDTalertTime.Style.StyleController := DMODskin.cxEditStyles1;

end;

procedure TFRAMEextShipDel.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDel.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDel.CHKcheckPreDelivClick(Sender: TObject);
var
  lv_EnableComps : Boolean;
begin
  inherited;
    // activar/desactivar componentes relacionados
  lv_EnableComps := CHKcheckPreDeliv.Checked;

  SetComponentesState(lv_EnableComps);

  if CDSEdition(CDStable)then
  begin

    if not lv_EnableComps then
    begin
      CDStable.FieldByName('alertDays').Clear;
      CDStable.FieldByName('alertTime').Clear;
      CDStable.FieldByName('csaEmails').Clear;

```

#### **FRextShipDel.dfm**

```
inherited FRAMEextShipDel: TFRAMEextShipDel
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLname: TsLabel [0]
    Left = 42
    Top = 41
    Width = 65
    Height = 13
    Caption = 'Alert Da&ys:'
    FocusControl = EDTalertDays
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 42
    Top = 67
    Width = 64
    Height = 13
    Caption = 'Alert T&ime:'
    FocusControl = EDTalertTime
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label2: TsLabel [2]
    Left = 42
    Top = 93
    Width = 71
    Height = 13
    Caption = 'CSA Emai&ls:'
    FocusControl = EDTcsaEmails
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 501
    TabOrder = 4
    Visible = False
    inherited PNLeditActions: TsPanel
      Width = 577
    end
  end
  object CHKcheckPreDeliv: TsDBCheckBox [4]
    Left = 8
    Top = 11
    Width = 163
    Height = 19
    Caption = '&PreDelivery Information'
    TabOrder = 0
    OnClick = CHKcheckPreDelivClick
    SkinData.SkinSection = 'CHECKBOX'
    ImgChecked = 0
    ImgUnchecked = 0
    DataField = 'checkPreDeliv'
    DataSource = DStable
    ValueChecked = 'Y'
    ValueUnchecked = 'N'
  end
  object EDTcsaEmails: TsDBEdit [5]
    Left = 116
    Top = 88
    Width = 385
    Height = 21
    AutoSize = False
    BevelWidth = 0
    Color = clWhite
    DataField = 'csaEmails'
    DataSource = DStable
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
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
  object EDTalertTime: TcxDBTimeEdit [6]
    Left = 116
```
<!-- tabs:end -->

