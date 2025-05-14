<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRextShipDelCons`

## 1. Overview:

### Main Objective:
The `FRextShipDelCons` code snippet defines a Delphi frame (`TFRAMEextShipDelCons`) that is part of a user interface for managing shipping delivery consignees. It provides functionality to configure alerts for shipping deliveries, including setting alert days, alert times, and CSA (Customer Service Agent) emails. The frame allows users to enable or disable these alert configurations dynamically.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP**: The code references SOAP-related units (`InvokeRegistry`, `SOAPHTTPClient`) for potential web service communication.
- **Database Components**: Uses `DB`, `DBClient`, and `DBCtrls` for database interaction.
- **Third-party Libraries**: Includes components from `cx` (DevExpress) and `s` (AlphaControls) libraries for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements**:
  - `CHKcheckPreDeliv` (Checkbox): Enables or disables alert configurations.
  - `EDTcsaEmails` (Text Field): Input for CSA emails.
  - `EDTalertTime` (Time Picker): Input for alert time.
  - `EDTalertDays` (Masked Edit): Input for alert days.
- **Form Actions**:
  - Clicking the checkbox (`CHKcheckPreDeliv`) toggles the state of the alert configuration fields.
  - Data is validated and saved when the user interacts with the form.

---

## 2. Functionality Description:

### User/Software Actions:
- Enable or disable alert configurations using the checkbox.
- Input alert days, alert time, and CSA emails.
- Automatically clear alert fields when the checkbox is unchecked.

### Main Components:
- **Checkbox (`CHKcheckPreDeliv`)**: Toggles the state of related fields.
- **Text Fields (`EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`)**: Accept user input for alert configurations.
- **Methods**:
  - `SetComponentesState`: Enables or disables fields based on the checkbox state.
  - `m_Validate`: Validates the form data.
  - `ShowData`: Displays data and sets the initial state of components.

### Pseudo-code:
- Checkbox Click Event:
  ```
  if checkbox clicked then
    toggle state of related fields
    if checkbox unchecked then
      clear alert fields
  ```
- Data Validation:
  ```
  if form data is valid then
    proceed with saving
  else
    show error message
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with default properties (`MasterKeyFields`, `DataPacketName`, etc.).
   - The action panel is hidden, and available actions are cleared.
   - The style of `EDTalertTime` is set using a skin manager (`DMODskin.cxEditStyles1`).

2. **User Interaction**:
   - The user toggles the checkbox to enable or disable alert configurations.
   - If enabled, the user can input alert days, time, and CSA emails.
   - If disabled, the fields are cleared automatically.

3. **Functions**:
   - `Create` (File: `FRextShipDelCons.pas`): Initializes the frame.
   - `ShowData` (File: `FRextShipDelCons.pas`): Displays data and sets the initial state of components.
   - `SetComponentesState` (File: `FRextShipDelCons.pas`): Toggles the state of fields.
   - `CHKcheckPreDelivClick` (File: `FRextShipDelCons.pas`): Handles checkbox click events.

### Required Data:
- Alert days (integer).
- Alert time (time format).
- CSA emails (string).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Enable Alert Configuration**:
  - Preconditions: None.
  - Action: Checkbox is checked, enabling related fields.
- **Disable Alert Configuration**:
  - Preconditions: None.
  - Action: Checkbox is unchecked, disabling and clearing related fields.

### Available Filters:
- No filters are explicitly defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- No explicit validations are defined in the code.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame with default properties and styles.
2. **`ShowData`**:
   - Displays data and sets the initial state of components based on the checkbox state.
3. **`SetComponentesState`**:
   - Enables or disables fields based on the checkbox state.
4. **`CHKcheckPreDelivClick`**:
   - Handles checkbox click events to toggle field states and clear data if necessary.

---

## 6. API Service Consumption:

- No API calls are explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- The fields `EDTalertDays`, `EDTalertTime`, and `EDTcsaEmails` are only enabled when the checkbox `CHKcheckPreDeliv` is checked.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress (`cx` components)**: Used for enhanced UI components like `TcxDBTimeEdit` and `TcxDBMaskEdit`.
- **AlphaControls (`s` components)**: Used for styled UI components like `TsLabel`, `TsDBCheckBox`, and `TsDBEdit`.

### Custom Components:
- **`TkneControls`**: Provides methods like `SetControlState` for enabling/disabling controls.

---

## 9. Fields and Validations Listing:

- **CHKcheckPreDeliv** (type: checkbox, optional): Toggles alert configurations.
- **EDTcsaEmails** (type: string, optional): Input for CSA emails.
- **EDTalertTime** (type: time, optional): Input for alert time.
- **EDTalertDays** (type: integer, optional): Input for alert days.

Mapping of displayed values and database columns:
- `EDTcsaEmails` → `csaEmails`.
- `EDTalertTime` → `alertTime`.
- `EDTalertDays` → `alertDays`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Display Data]
   --> [User Toggles Checkbox] --> [Enable/Disable Fields]
   --> [Save Data] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Checkbox: Click
Checkbox --> Frame: Toggle Fields
Frame --> Database: Save Data
```

### Code Snippets:
```delphi
procedure TFRAMEextShipDelCons.CHKcheckPreDelivClick(Sender: TObject);
begin
  SetComponentesState(CHKcheckPreDeliv.Checked);
  if not CHKcheckPreDeliv.Checked then
  begin
    CDStable.FieldByName('alertDays').Clear;
    CDStable.FieldByName('alertTime').Clear;
    CDStable.FieldByName('csaEmails').Clear;
  end;
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Verdana; color: #4D4D4D;">
  <label for="alertDays">Alert Days:</label>
  <input type="text" id="alertDays" style="margin-bottom: 10px;"><br>
  <label for="alertTime">Alert Time:</label>
  <input type="time" id="alertTime" style="margin-bottom: 10px;"><br>
  <label for="csaEmails">CSA Emails:</label>
  <input type="email" id="csaEmails" style="margin-bottom: 10px;"><br>
  <input type="checkbox" id="checkPreDeliv"> Enable Alerts
</div>
```

---

## 11. Important Comments in the Code:

- `SetComponentesState`: Critical for enabling/disabling fields dynamically.
- `CHKcheckPreDelivClick`: Handles the main logic for toggling alert configurations.

---

## 12. Conclusion:

The `FRextShipDelCons` frame provides a simple and effective way to manage shipping delivery alerts. While it lacks explicit error handling and validations, it is well-structured and integrates seamlessly with database components and third-party libraries.

---

## 13. Short Summary:

The `FRextShipDelCons` frame manages shipping delivery alerts, allowing users to configure alert days, times, and CSA emails dynamically. It uses Delphi's VCL and third-party libraries for enhanced UI and database interaction.#### **FRextShipDelCons.pas**

```
unit FRextShipDelCons;

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
  TFRAMEextShipDelCons = class(TFRAMEBaseCtrlEditSOA)
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
  FRAMEextShipDelCons: TFRAMEextShipDelCons;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDelCons }

constructor TFRAMEextShipDelCons.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode=cons';
  DataPacketName  := 'ShipDelConsignee';
  PropertyName    := 'shipDelConsignee';
  FrameType       := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  EDTalertTime.Style.StyleController := DMODskin.cxEditStyles1;

end;

procedure TFRAMEextShipDelCons.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDelCons.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDelCons.CHKcheckPreDelivClick(Sender: TObject);
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

#### **FRextShipDelCons.dfm**

```
inherited FRAMEextShipDelCons: TFRAMEextShipDelCons
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
```
<!-- tabs:end -->

