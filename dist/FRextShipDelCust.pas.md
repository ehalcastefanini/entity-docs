<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRextShipDelCust`

## 1. Overview:

### Main Objective:
The `FRextShipDelCust` code snippet defines a Delphi frame (`TFRAMEextShipDelCust`) that is used to manage and display shipping delivery customer-related data. It provides a user interface for configuring alert settings, such as alert days, alert time, and CSA emails, with the ability to enable or disable these fields based on a checkbox selection.

### Technologies Used:
- **Delphi VCL (Visual Component Library):** Used for creating the user interface and handling events.
- **SOAP/HTTP Client:** For potential integration with external services.
- **Database Components:** For managing and interacting with datasets.
- **Third-party Libraries:** Includes `TsLabel`, `TsDBCheckBox`, `TsDBEdit`, and other components for enhanced UI functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements:**
  - `CHKcheckPreDeliv` (Checkbox): Enables or disables related fields.
  - `EDTcsaEmails` (Text Field): Input for CSA emails.
  - `EDTalertTime` (Time Field): Input for alert time.
  - `EDTalertDays` (Mask Edit Field): Input for alert days.
- **Form Actions:**
  - Checkbox click (`CHKcheckPreDelivClick`): Toggles the state of related fields.
  - Dataset edit (`CDStableAfterEdit`): Handles changes in the dataset.

---

## 2. Functionality Description:

### User/Software Actions:
- Enable or disable alert-related fields by toggling the `CHKcheckPreDeliv` checkbox.
- Input values for alert days, alert time, and CSA emails.
- Automatically clear alert-related fields when the checkbox is unchecked.

### Main Components:
- **Checkbox (`CHKcheckPreDeliv`):** Controls the state of related fields.
- **Text Fields (`EDTcsaEmails`, `EDTalertTime`, `EDTalertDays`):** Allow users to input specific data.
- **Dataset (`CDStable`):** Manages the data being edited or displayed.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHKcheckPreDeliv`:
  ```
  if checkbox is checked then
    enable related fields
  else
    disable related fields and clear their values
  ```
- `AfterEdit` event of `CDStable`:
  ```
  if dataset is edited then
    perform necessary updates
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The frame is initialized with default properties (`MasterKeyFields`, `DataPacketName`, etc.).
   - The action panel is hidden, and available actions are cleared.
   - The style controller for `EDTalertTime` is set.

2. **User Interaction:**
   - When the checkbox (`CHKcheckPreDeliv`) is clicked, the state of related fields is toggled.
   - If the checkbox is unchecked, the related fields are cleared.

3. **Functions and File Locations:**
   - `Create` (Initialization): Defined in `FRextShipDelCust`.
   - `ShowData` (Display Data): Defined in `FRextShipDelCust`.
   - `SetComponentesState` (Toggle Field States): Defined in `FRextShipDelCust`.
   - `CHKcheckPreDelivClick` (Checkbox Click Event): Defined in `FRextShipDelCust`.

### Required Data:
- Alert days, alert time, and CSA emails (if the checkbox is checked).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Enable/Disable Fields:**
  - Action: Toggle the state of alert-related fields.
  - Precondition: Checkbox (`CHKcheckPreDeliv`) must be clicked.

- **Clear Fields:**
  - Action: Clear alert-related fields.
  - Precondition: Checkbox (`CHKcheckPreDeliv`) must be unchecked.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- No explicit validations are defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`Create`:** Initializes the frame with default properties and configurations.
2. **`ShowData`:** Displays data and sets the state of components based on the checkbox value.
3. **`SetComponentesState`:** Enables or disables related fields based on the checkbox state.
4. **`CHKcheckPreDelivClick`:** Handles the click event of the checkbox and updates the state of related fields.

---

## 6. API Service Consumption:

- No API service calls are explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Field:** Alert days, alert time, and CSA emails.
- **Condition:** These fields are enabled only when the `CHKcheckPreDeliv` checkbox is checked.

---

## 8. Dependencies:

### External Libraries:
- **`TsLabel`, `TsDBCheckBox`, `TsDBEdit`:** Used for UI components.
- **`cxDBEdit`, `cxTimeEdit`:** Used for advanced input fields.
- **`TkneControls`:** Used for managing control states.

### Custom Components:
- **`TFRAMEBaseCtrlEditSOA`:** Base frame class providing inherited functionality.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Alert Days (`EDTalertDays`):**
   - Type: Mask Edit.
   - Required: Not explicitly defined.
2. **Alert Time (`EDTalertTime`):**
   - Type: Time Edit.
   - Required: Not explicitly defined.
3. **CSA Emails (`EDTcsaEmails`):**
   - Type: Text Edit.
   - Required: Not explicitly defined.

### Mapping:
- No explicit mapping to database columns is defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [User Clicks Checkbox]
    --> [Toggle Field States] --> [Clear Fields if Unchecked] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Checkbox: Click
Checkbox --> Frame: Trigger Event
Frame --> Fields: Enable/Disable
```

### Code Snippets:
```delphi
procedure TFRAMEextShipDelCust.CHKcheckPreDelivClick(Sender: TObject);
begin
  SetComponentesState(CHKcheckPreDeliv.Checked);
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="font-family: Verdana; color: #4D4D4D;">
  <label for="alertDays">Alert Days:</label>
  <input type="text" id="alertDays" style="margin-left: 10px;"><br>
  <label for="alertTime">Alert Time:</label>
  <input type="time" id="alertTime" style="margin-left: 10px;"><br>
  <label for="csaEmails">CSA Emails:</label>
  <input type="email" id="csaEmails" style="margin-left: 10px;"><br>
  <input type="checkbox" id="checkPreDeliv"> Enable Alerts
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization:**
  - Sets default properties and hides the action panel.
- **Checkbox Click Event:**
  - Toggles the state of related fields and clears them if unchecked.

---

## 12. Conclusion:

The `FRextShipDelCust` frame provides a simple and effective way to manage shipping delivery customer alert settings. While it is functional, the code lacks explicit error handling, field validations, and default values, which could improve its robustness.

---

## 13. Short Summary:

The `FRextShipDelCust` frame manages alert settings for shipping delivery customers, allowing users to enable or disable fields based on a checkbox. It initializes with default properties and provides a clean interface for managing alert days, time, and CSA emails.#### **FRextShipDelCust.pas**

```
unit FRextShipDelCust;

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
  TFRAMEextShipDelCust = class(TFRAMEBaseCtrlEditSOA)
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
  FRAMEextShipDelCust: TFRAMEextShipDelCust;

implementation

uses kneFREditSOA, kneTypes, kneUtils, Global, DMskin;

{$R *.dfm}

{ TFRAMEextShipDelCust }

constructor TFRAMEextShipDelCust.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=cust';
  DataPacketName  := 'ShipDelCustomer';
  PropertyName    := 'shipDelCustomer';
  FrameType       := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  EDTalertTime.Style.StyleController := DMODskin.cxEditStyles1;

end;

procedure TFRAMEextShipDelCust.ShowData;
begin
  inherited;

  SetComponentesState(CHKcheckPreDeliv.Checked);
end;

procedure TFRAMEextShipDelCust.SetComponentesState(pv_State: Boolean);
begin
  TkneControls.SetControlState(EDTalertDays, pv_State);
  TkneControls.SetControlState(EDTalertTime, pv_State);
  TkneControls.SetControlState(EDTcsaEmails, pv_State);
end;

procedure TFRAMEextShipDelCust.CHKcheckPreDelivClick(Sender: TObject);
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

#### **FRextShipDelCust.dfm**

```
inherited FRAMEextShipDelCust: TFRAMEextShipDelCust
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

