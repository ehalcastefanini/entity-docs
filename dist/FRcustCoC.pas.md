<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustCoC` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustCoC` code unit defines a form (`TFRAMEcustCoC`) that manages certification data for three types of certifications: FSC, PEFC, and CW. It allows users to input, validate, and manage certification details such as certification numbers, license numbers, and validity dates. The form also provides functionality to enable or disable fields based on user interactions with checkboxes.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **SOAP Services**: Used for potential external service calls (though not explicitly defined in the provided code).
- **Database Components**: Includes `TsDBEdit`, `TsDBCheckBox`, and `TcxDBDateEdit` for binding form fields to a database.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `TsDBEdit`: Text input fields for certification and license numbers.
  - `TcxDBDateEdit`: Date input fields for start and end dates.
  - `TsDBCheckBox`: Checkboxes for enabling/disabling certification types.
  - `TsLabel`: Labels for field descriptions.
  - `TsGroupBox`: Group boxes for organizing certification sections.
- **Form Actions and Effects**:
  - Clicking checkboxes enables or disables related fields.
  - Validation is performed before saving data.

---

## 2. Functionality Description:

### User Actions:
- Enable or disable certification sections (FSC, PEFC, CW) by interacting with checkboxes.
- Input certification numbers, license numbers, and validity dates.
- Validate the form data before saving.

### Main Components:
- **Group Boxes (`TsGroupBox`)**: Organize fields for FSC, PEFC, and CW certifications.
- **Checkboxes (`TsDBCheckBox`)**: Control the activation of related fields.
- **Date Editors (`TcxDBDateEdit`)**: Allow users to input start and end dates.
- **Text Editors (`TsDBEdit`)**: Allow users to input certification and license numbers.

### Pseudo-code for Actions and Events:
- `OnClick` event of `CHBnormaFsc`:
  ```pseudo
  if checkbox clicked then
    enable or disable related FSC fields
  ```
- `OnClick` event of `CHBnormaPefc`:
  ```pseudo
  if checkbox clicked then
    enable or disable related PEFC fields
  ```
- `OnClick` event of `CHBfscCw`:
  ```pseudo
  if checkbox clicked then
    enable or disable related CW fields
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default settings in the `Create` constructor.
   - Action panels are hidden, and no actions are available by default.
2. **User Interaction**:
   - Users interact with checkboxes to enable or disable fields.
   - Users input data into text and date fields.
3. **Validation**:
   - The `m_Validate` function ensures all required fields are correctly filled before saving.

### Data Requirements:
- Certification numbers, license numbers, and validity dates for FSC, PEFC, and CW certifications.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Enable/Disable Fields**:
  - Preconditions: The corresponding checkbox must be checked to enable fields.
  - Actions: Enable or disable related fields (certification number, license number, start date, end date).
- **Validation**:
  - Preconditions: All required fields must be filled and valid.

### Available Filters:
- No explicit filters are defined in the provided code.

### Error Messages:
- No explicit error messages are defined in the provided code.

### Default Field Values:
- No default values are explicitly defined in the provided code.

### Field Validation and Conditions:
- Validation logic is implemented in the `m_Validate` function, but specific conditions are not detailed in the provided code.

---

## 5. Main Functions:

### Functions:
1. **`Create` Constructor**:
   - Initializes the form with default settings.
   - Hides the action panel and disables actions.
2. **`CHBnormaFscClick`**:
   - Enables or disables FSC-related fields based on the checkbox state.
3. **`CHBnormaPefcClick`**:
   - Enables or disables PEFC-related fields based on the checkbox state.
4. **`CHBfscCwClick`**:
   - Enables or disables CW-related fields based on the checkbox state.
5. **`m_Validate`**:
   - Validates the form data before saving.

---

## 6. API Service Consumption:

No explicit API service calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- **Conditional Field**: FSC, PEFC, and CW fields are enabled only when their respective checkboxes are checked.
- **Conditions**:
  - FSC fields are visible and editable only when `CHBnormaFsc` is checked.
  - PEFC fields are visible and editable only when `CHBnormaPefc` is checked.
  - CW fields are visible and editable only when `CHBfscCw` is checked.

---

## 8. Dependencies:

### External Libraries:
- **Delphi VCL Components**: Used for form creation and UI elements.
- **SOAP Components**: Potentially used for external service calls.

### Custom Components:
- `TFRAMEBaseCtrlEditSOA`: Base class for the form, providing shared functionality.

---

## 9. Fields and Validations Listing:

### Fields:
1. **FSC Fields**:
   - Certification Number (`EDTcertifNumFsc`): Type: string, required.
   - License Number (`EDTlicenseFsc`): Type: string, required.
   - Start Date (`DTEstartDateFsc`): Type: date, required.
   - End Date (`DTEendDateFsc`): Type: date, required.
2. **PEFC Fields**:
   - Certification Number (`EDTcertifNumPefc`): Type: string, required.
   - License Number (`EDTlicensePefc`): Type: string, required.
   - Start Date (`DTEstartDatePefc`): Type: date, required.
   - End Date (`DTEendDatePefc`): Type: date, required.
3. **CW Fields**:
   - Certification Number (`EDTcertifNumCw`): Type: string, required.
   - License Number (`EDTlicenseCw`): Type: string, required.
   - Start Date (`DTEstartDateCw`): Type: date, required.
   - End Date (`DTEendDateCw`): Type: date, required.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [User Interacts with Checkboxes] --> [Enable/Disable Fields] --> [Validate Data] --> [Save Data]
```

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcustCoC.CHBfscCwClick(Sender: TObject);
begin
  inherited;
  EDTcertifNumCw.Enabled := CHBfscCw.Checked;
  EDTlicenseCw.Enabled := CHBfscCw.Checked;
  DTEstartDateCw.Enabled := CHBfscCw.Checked;
  DTEendDateCw.Enabled := CHBfscCw.Checked;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- The `Create` constructor initializes the form and hides the action panel.
- The `CHBfscCwClick` method dynamically enables or disables CW-related fields.

---

## 12. Conclusion:

The `FRcustCoC` code unit provides a structured form for managing certification data. Its strengths include dynamic field enabling/disabling and validation logic. However, the lack of explicit error messages and detailed validation rules limits its robustness.

---

## 13. Short Summary:

The `FRcustCoC` form manages FSC, PEFC, and CW certifications, enabling dynamic field control and validation. It is part of a larger system for certification management, with potential integration for database and SOAP services.#### **FRcustCoC.pas**

```
unit FRcustCoC;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRStatusInfo, cxDBEdit, sDBEdit, sLabel, sFrameAdapter, sBitBtn,
  sPanel, cxGraphics, kneFRFindEditSOA, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, sCheckBox,
  sDBCheckBox, sButton, cxCheckBox, sGroupBox, cxCalendar, sBevel;

type
  TFRAMEcustCoC = class(TFRAMEBaseCtrlEditSOA)
    GRPfsc: TsGroupBox;
    LBLstartDateFsc: TsLabel;
    LBLendDateFsc: TsLabel;
    EDTcertifNumFsc: TsDBEdit;
    LBLcertifNumFsc: TsLabel;
    DTEstartDateFsc: TcxDBDateEdit;
    DTEendDateFsc: TcxDBDateEdit;
    CHBnormaFsc: TsDBCheckBox;
    LBLchbNormaFsc: TsLabel;
    GRPpefc: TsGroupBox;
    LBLchbNormaPefc: TsLabel;
    CHBnormaPefc: TsDBCheckBox;
    EDTcertifNumPefc: TsDBEdit;
    LBLcertifNumPefc: TsLabel;
    LBLstartDatePefc: TsLabel;
    DTEstartDatePefc: TcxDBDateEdit;
    LBLendDatePefc: TsLabel;
    DTEendDatePefc: TcxDBDateEdit;
    GRPcw: TsGroupBox;
    LBLstartDateCw: TsLabel;
    LBLendDateCw: TsLabel;
    LBLcertifNumCw: TsLabel;
    LBLfscCntrlWood: TsLabel;
    CHBfscCw: TsDBCheckBox;
    EDTcertifNumCw: TsDBEdit;
    DTEstartDateCw: TcxDBDateEdit;
    DTEendDateCw: TcxDBDateEdit;
    LBLlicenseCw: TsLabel;
    EDTlicenseCw: TsDBEdit;
    LBLlicenseFsc: TsLabel;
    EDTlicenseFsc: TsDBEdit;
    LBLlicensePefc: TsLabel;
    EDTlicensePefc: TsDBEdit;
    procedure CHBnormaFscClick(Sender: TObject);
    procedure CHBnormaPefcClick(Sender: TObject);
    procedure ShowData; override;
    procedure CHBfscCwClick(Sender: TObject);
  private
    { Private declarations }
  protected
    function  m_Validate: Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEcustCoC: TFRAMEcustCoC;

implementation

uses kneTypes, kneFREditSOA, kneUtils, DMskin, Global, CurrencyServiceUtils;

{$R *.dfm}

{ TFRAMEcustIBAN }

constructor TFRAMEcustCoC.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  FrameType := frtGhost;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
end;

procedure TFRAMEcustCoC.CHBfscCwClick(Sender: TObject);
begin
  inherited;

  if not Assigned(EDTcertifNumCw) then
    Exit;

  EDTcertifNumCw.Enabled := CHBfscCw.Checked;
  EDTlicenseCw.Enabled   := CHBfscCw.Checked;
  DTEstartDateCw.Enabled := CHBfscCw.Checked;
  DTEendDateCw.Enabled   := CHBfscCw.Checked;

  if not CHBfscCw.Checked then
```

#### **FRcustCoC.dfm**

```
inherited FRAMEcustCoC: TFRAMEcustCoC
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object GRPfsc: TsGroupBox [0]
    Left = 8
    Top = 106
    Width = 553
    Height = 91
    Caption = '              '
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    SkinData.SkinSection = 'GROUPBOX'
    object LBLstartDateFsc: TsLabel
      Left = 16
      Top = 57
      Width = 64
      Height = 13
      Caption = 'Start Date:'
      FocusControl = DTEstartDateFsc
      ParentFont = False
      Font.Charset = ANSI_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
    end
    object LBLendDateFsc: TsLabel
      Left = 266
      Top = 57
      Width = 57
      Height = 13
      Caption = 'End Date:'
      FocusControl = DTEendDateFsc
      ParentFont = False
      Font.Charset = ANSI_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
    end
    object LBLcertifNumFsc: TsLabel
      Left = 16
      Top = 25
      Width = 86
      Height = 13
      Caption = 'Certif Number:'
      FocusControl = EDTcertifNumFsc
      ParentFont = False
      Font.Charset = ANSI_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
    end
    object LBLchbNormaFsc: TsLabel
      Left = 33
      Top = 0
      Width = 23
      Height = 13
      Caption = 'FSC'
      ParentFont = False
      Font.Charset = ANSI_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
    end
    object LBLlicenseFsc: TsLabel
      Left = 266
      Top = 25
      Width = 96
      Height = 13
      Caption = 'License Number:'
      FocusControl = EDTlicenseFsc
      ParentFont = False
      Font.Charset = ANSI_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
    end
    object CHBnormaFsc: TsDBCheckBox
      Left = 12
      Top = 0
      Width = 17
      Height = 15
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      TabOrder = 0
      OnClick = CHBnormaFscClick
      SkinData.SkinSection = 'CHECKBOX'
      ImgChecked = 0
      ImgUnchecked = 0
      DataField = 'normaFsc'
```
<!-- tabs:end -->

