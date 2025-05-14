<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRextShipDelConsMkt`

## 1. Overview:

### Objective and Problem Solved:
The `TFRAMEextShipDelConsMkt` class is a Delphi form designed to manage and display pre-delivery alert configurations for a shipping delivery system. It allows users to configure alert days, alert times, and CSA (Customer Service Agent) emails, with the ability to enable or disable these settings based on a checkbox (`CHKcheckPreDeliv`). This form ensures that users can easily manage pre-delivery notifications, improving operational efficiency.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For potential integration with external systems.
- **Database Components**: For managing and binding data to the form fields.
- **Third-party Libraries**: Includes `cxControls`, `cxEdit`, and `sSkin` components for enhanced UI/UX.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `CHKcheckPreDeliv` (Checkbox): Enables or disables pre-delivery information.
  - `EDTcsaEmails` (Text Input): Input for CSA emails.
  - `EDTalertTime` (Time Input): Input for alert time.
  - `EDTalertDays` (Masked Input): Input for alert days.
- **Form Actions and Effects**:
  - Clicking the checkbox (`CHKcheckPreDeliv`) enables or disables the related fields (`EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`).
  - Data changes in the form trigger database updates.

---

## 2. Functionality Description:

### User/Software Actions:
- Enable or disable pre-delivery information using the checkbox.
- Input or clear alert days, alert time, and CSA emails.
- Automatically clear related fields when pre-delivery is disabled.

### Main Components:
- **Checkbox (`CHKcheckPreDeliv`)**: Toggles the state of related fields.
- **Text Input (`EDTcsaEmails`)**: Accepts CSA email addresses.
- **Time Input (`EDTalertTime`)**: Accepts alert time.
- **Masked Input (`EDTalertDays`)**: Accepts alert days.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHKcheckPreDeliv`:
  ```pseudo
  if checkbox is checked then
      enable related fields
  else
      disable related fields
      clear related fields
  ```
- `OnChange` event of fields:
  ```pseudo
  if field value changes then
      validate field
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings in the `Create` constructor.
   - The `ShowData` method is called to set the initial state of components based on the checkbox value.
2. **User Interaction**:
   - Users interact with the checkbox and input fields.
   - Events like `OnClick` and `AfterEdit` are triggered to update the UI and database.
3. **Functions**:
   - `SetComponentesState`: Enables or disables fields based on the checkbox state.
   - `CHKcheckPreDelivClick`: Handles the checkbox click event.
   - `CDStableAfterEdit`: Updates the database after editing.

### Required Data:
- Checkbox state (`CHKcheckPreDeliv`).
- Alert days (`EDTalertDays`).
- Alert time (`EDTalertTime`).
- CSA emails (`EDTcsaEmails`).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Enable/Disable Fields**:
  - Action: Toggle fields based on the checkbox.
  - Precondition: Checkbox must be checked to enable fields.
- **Clear Fields**:
  - Action: Clear fields when the checkbox is unchecked.
  - Precondition: Checkbox must be unchecked.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Checkbox (`CHKcheckPreDeliv`): Default unchecked.
- Other fields: No default values defined.

### Field Validation and Conditions:
- `EDTcsaEmails`: Should accept valid email addresses (not explicitly validated in the code).
- `EDTalertTime`: Should accept valid time input.
- `EDTalertDays`: Should accept valid numeric input.

---

## 5. Main Functions:

### Functions:
1. **`Create`**:
   - Initializes the form and sets default properties.
2. **`ShowData`**:
   - Updates the state of components based on the checkbox value.
3. **`SetComponentesState`**:
   - Enables or disables fields based on a boolean parameter.
4. **`CHKcheckPreDelivClick`**:
   - Handles the checkbox click event and updates the state of related fields.
5. **`CDStableAfterEdit`**:
   - Updates the database after editing fields.

---

## 6. API Service Consumption:

No explicit API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`.
- **Condition**: These fields are only enabled when `CHKcheckPreDeliv` is checked.

---

## 8. Dependencies:

### External Libraries:
- `cxControls`, `cxEdit`: For enhanced UI components.
- `sSkin`: For skinning and styling the form.

### Custom Components:
- `TkneControls`: Used for setting the state of controls.

---

## 9. Fields and Validations Listing:

- **CHKcheckPreDeliv** (Checkbox):
  - Type: Boolean.
  - Required: No.
  - Default: Unchecked.
- **EDTcsaEmails** (Text Input):
  - Type: String.
  - Required: No.
  - Validation: Not explicitly defined.
- **EDTalertTime** (Time Input):
  - Type: Time.
  - Required: No.
  - Validation: Not explicitly defined.
- **EDTalertDays** (Masked Input):
  - Type: Numeric.
  - Required: No.
  - Validation: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [ShowData] --> [User Interaction] --> [Update Fields/Database] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Checkbox: Click
Checkbox --> Form: Update State
Form --> Database: Update Data
```

### Code Snippets:
```delphi
procedure TFRAMEextShipDelConsMkt.CHKcheckPreDelivClick(Sender: TObject);
begin
  SetComponentesState(CHKcheckPreDeliv.Checked);
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Verdana; color: #5059883;">
  <label>Alert Days:</label>
  <input type="text" placeholder="Enter alert days">
  <label>Alert Time:</label>
  <input type="time">
  <label>CSA Emails:</label>
  <input type="email" placeholder="Enter CSA emails">
  <input type="checkbox"> PreDelivery Information
</div>
```

---

## 11. Important Comments in the Code:

- `SetComponentesState`: Critical for enabling/disabling fields.
- `CHKcheckPreDelivClick`: Handles the main logic for toggling fields.

---

## 12. Conclusion:

The `TFRAMEextShipDelConsMkt` form provides a user-friendly interface for managing pre-delivery alerts. Its strengths include dynamic field toggling and integration with database components. However, it lacks explicit validation and error handling, which could be improved.

---

## 13. Short Summary:

The `TFRAMEextShipDelConsMkt` form manages pre-delivery alert configurations, allowing users to enable/disable fields dynamically and update related data. It integrates with database components and provides a simple, efficient interface for managing shipping notifications.#### **FRextShipDelConsMkt.pas**

```
unit FRextShipDelConsMkt;

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
  TFRAMEextShipDelConsMkt = class(TFRAMEBaseCtrlEditSOA)
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
  FRAMEextShipDelConsMkt: TFRAMEextShipDelConsMkt;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDelConsMkt }

constructor TFRAMEextShipDelConsMkt.Create(AOwner: TComponent);
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

procedure TFRAMEextShipDelConsMkt.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDelConsMkt.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDelConsMkt.CHKcheckPreDelivClick(Sender: TObject);
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

#### **FRextShipDelConsMkt.dfm**

```
inherited FRAMEextShipDelConsMkt: TFRAMEextShipDelConsMkt
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
    Width = 721
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
    Width = 605
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

