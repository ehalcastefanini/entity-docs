<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocsCheckDefaults`

## 1. Overview:

### Objective and Problem Solved:
The `FRdocsCheckDefaults` unit defines a Delphi frame (`TFRAMEdocsCheckDefaults`) that provides a user interface for managing and configuring default settings related to "Consignee" and "Consignee Market." It allows users to select and configure these entities through a form-based interface. The frame integrates with data sources and services to fetch and display relevant information.

This frame is particularly useful in scenarios where users need to manage default configurations for documents or entities in a system, ensuring consistency and reducing manual input errors.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services (`TCheckListDocsDefaultServiceUtils`).
- **Database Components**: For binding data to UI elements (`DBClient`, `DBCtrls`).
- **Custom Components**: Includes custom components like `TFRAMEFindEditSOA`, `TsLabel`, and `TsPanel`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `LBLconsMkt` (Label): Displays "Consignee Market."
  - `Label1` (Label): Displays "Consignee."
  - `FRAMEfindConsignee` (Custom Frame): Allows users to search and select a "Consignee."
  - `FRAMEfindConsigneeMarket` (Custom Frame): Allows users to search and select a "Consignee Market."
- **Form Actions and Effects**:
  - `m_SetFindConsignee`: Configures the "Consignee" search frame.
  - `m_SetFindConsMarket`: Configures the "Consignee Market" search frame.
  - `m_SetAccessMode`: Adjusts the access mode and enables/disables frames based on the mode.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can select and configure "Consignee" and "Consignee Market" using the provided frames.
- The frame dynamically adjusts its behavior based on the access mode (`NEW` or other modes).

### Main Components:
1. **Labels (`LBLconsMkt`, `Label1`)**: Display static text for "Consignee Market" and "Consignee."
2. **Frames (`FRAMEfindConsignee`, `FRAMEfindConsigneeMarket`)**: Custom frames for searching and selecting entities.
3. **Service Integration (`TCheckListDocsDefaultServiceUtils`)**: Handles interactions with external services.

### Pseudo-code for Actions and Events:
- `OnCreate` event of the frame:
  ```
  if frame is created then
    initialize properties
    configure service and data source
    call m_SetFindConsignee
    call m_SetFindConsMarket
  ```
- `m_SetAccessMode` procedure:
  ```
  if access mode is 'NEW' then
    enable FRAMEfindConsigneeMarket
    enable FRAMEfindConsignee
  else
    disable FRAMEfindConsigneeMarket
    disable FRAMEfindConsignee
  ```
- `m_SetFindConsMarket` procedure:
  ```
  configure FRAMEfindConsigneeMarket with data source and field mappings
  set FindDialog properties for "Consignee Market"
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created, and its properties are initialized.
   - The `TCheckListDocsDefaultServiceUtils` service is instantiated.
   - The `m_SetFindConsignee` and `m_SetFindConsMarket` procedures are called to configure the frames.
2. **User Interaction**:
   - Users interact with the "Consignee" and "Consignee Market" frames to select entities.
   - The `m_SetAccessMode` procedure adjusts the frame's behavior based on the access mode.

### Data Requirements:
- Users must provide/select:
  - A "Consignee Market" (via `FRAMEfindConsigneeMarket`).
  - A "Consignee" (via `FRAMEfindConsignee`).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Action**: Enable/Disable frames based on access mode.
  - **Precondition**: Access mode must be set to `NEW` to enable the frames.
- **Action**: Configure "Consignee" and "Consignee Market" frames.
  - **Precondition**: Data source and field mappings must be correctly set.

### Available Filters:
- "Consignee Market" filter:
  - Field: `marketCode` (Code).
  - Field: `description` (Description).
- "Consignee" filter:
  - Not explicitly defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- "Consignee Market" and "Consignee" fields must be correctly mapped to the data source.
- No additional validations are explicitly defined in the code.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame and its properties.
   - Configures the service and frames.

2. **`m_SetAccessMode`**:
   - Adjusts the frame's behavior based on the access mode.

3. **`m_SetFindConsignee`**:
   - Configures the "Consignee" frame.

4. **`m_SetFindConsMarket`**:
   - Configures the "Consignee Market" frame.

---

## 6. API Service Consumption:

- **Service Name**: `TCheckListDocsDefaultServiceUtils`.
- **Purpose**: Provides data and functionality for managing default document settings.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- The "Consignee Market" and "Consignee" frames are enabled only when the access mode is set to `NEW`.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components**: For service integration.
- **Database Components**: For data binding.
- **Custom Components**: `TFRAMEFindEditSOA`, `TsLabel`, `TsPanel`.

### Custom Components:
- `TFRAMEFindEditSOA`: Used for searching and selecting entities.

---

## 9. Fields and Validations Listing:

- **Consignee Market**:
  - Type: String.
  - Required: Yes.
  - Field Mappings: `marketCode` (Code), `description` (Description).
- **Consignee**:
  - Type: String.
  - Required: Yes.
  - Field Mappings: Not explicitly defined.

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not define a complex workflow.)

### Sequence Diagram:
(Not applicable as the interactions are limited to the frame and its components.)

### Code Snippets:
```delphi
procedure TFRAMEdocsCheckDefaults.m_SetFindConsMarket;
begin
  with FRAMEfindConsigneeMarket do
  begin
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'consMkt';
    EditSettings.FieldNameForDesc := 'marketDescrip';
    FindDialog.Caption := 'Consignee Market Selection';
  end;
end;
```

### Screenshots:
(Not applicable as the DFM file is not fully provided.)

---

## 11. Important Comments in the Code:

- The `Create` constructor contains critical initialization logic for the frame.
- The `m_SetAccessMode` procedure adjusts the frame's behavior dynamically.

---

## 12. Conclusion:

The `FRdocsCheckDefaults` unit provides a robust framework for managing "Consignee" and "Consignee Market" configurations. While it effectively integrates with services and data sources, the lack of explicit error handling and validations may limit its usability in complex scenarios.

---

## 13. Short Summary:

The `FRdocsCheckDefaults` unit defines a Delphi frame for managing "Consignee" and "Consignee Market" configurations, integrating with services and data sources to provide a user-friendly interface for selecting and configuring these entities.#### **FRdocsCheckDefaults.pas**

```
unit FRdocsCheckDefaults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, kneFRStatusInfo, kneFRGridEditSOA, sDBEdit, sLabel,
  sFrameAdapter, sBitBtn, sPanel, DMskin, cxGraphics, cxDBEdit, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxImageComboBox, sDBComboBox, sBevel;

type
  TFRAMEdocsCheckDefaults = class(TFRAMEBaseCtrlEditSOA)
    LBLconsMkt: TsLabel;
    Label1: TsLabel;
    FRAMEfindConsignee: TFRAMEFindEditSOA;
    FRAMEfindConsigneeMarket: TFRAMEFindEditSOA;
  private
    procedure m_SetFindConsignee;
    procedure m_SetFindConsMarket;
    procedure m_SetAccessMode(Sender: TObject; var pv_state: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

  end;

var
  FRAMEdocsCheckDefaults: TFRAMEdocsCheckDefaults;

implementation

uses
  kneUtils, kneTypes, Global, kneFGFindUtils, kneFindDialogSOA,
  //---
  CheckListDocsDefaultServiceUtils, ConsigneeMarketServiceUtils,
  kneFREditSOA;

{$R *.dfm}

{ TFRAMEdocsCheckDefaults }

constructor TFRAMEdocsCheckDefaults.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';          // n�o necessita de estar def. na frame Master
  DataPacketName := 'CheckListDocDefMaster';
  PropertyName := '';             // n�o necessita de estar def. na frame Master
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCheckListDocsDefaultServiceUtils.Create(self);


  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
//  FRAMEstatusInfo1.DataSource := DStable;

  OnSetAccessMode := m_SetAccessMode;

  m_SetFindConsignee;
  m_SetFindConsMarket;
end;


Procedure TFRAMEdocsCheckDefaults.m_SetAccessMode(Sender: TObject;var pv_state: Boolean);
begin
//  if (not assigned(CDStable)) or (not CDStable.Active) then   //JAR 10-02-2010  Aqui N�o � necess�ria a protec��o, pois n�o acede ao datset
//    exit;

  FRAMEfindConsigneeMarket.Enable := AccessMode = 'NEW';
  FRAMEfindConsignee.Enable := AccessMode = 'NEW';
end;

procedure TFRAMEdocsCheckDefaults.m_SetFindConsMarket;
begin
  with FRAMEfindConsigneeMarket do
  begin
    EditSettings.DataSource := nil;
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'consMkt';
    EditSettings.FieldNameForDesc := 'marketDescrip';

    // configura��o do Find Dialog
    FindDialog.Caption := 'Consignee Market Selection';
    FindDialog.Options.DataSelection.FieldNameForCode := 'marketCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
    FindDialog.Options.DataSelection.FieldNamesForDesc.Add('description');

```

#### **FRdocsCheckDefaults.dfm**

```
inherited FRAMEdocsCheckDefaults: TFRAMEdocsCheckDefaults
  object LBLconsMkt: TsLabel [0]
    Left = 8
    Top = 13
    Width = 65
    Height = 13
    Hint = 'Consignee Market'
    Caption = 'Cons.Market:'
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 8
    Top = 42
    Width = 54
    Height = 13
    Hint = 'Consignee'
    Caption = 'Consignee:'
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 227
    Width = 580
    TabOrder = 2
    Visible = False
    inherited PNLeditActions: TsPanel
      Width = 787
    end
  end
  inline FRAMEfindConsignee: TFRAMEFindEditSOA [3]
    Left = 85
    Top = 37
    Width = 495
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    inherited PNLdesc: TPanel
      Left = 97
      Width = 398
      DesignSize = (
        398
        21)
      inherited DBEDesc: TsDBEdit
        Width = 398
      end
      inherited EDDesc: TsEdit
        Width = 398
      end
    end
    inherited PNLcode: TPanel
      Width = 97
      DesignSize = (
        97
        21)
      inherited DBE: TsDBEdit
        Width = 76
      end
      inherited FE: TsMaskEdit
        Width = 76
      end
      inherited PNLbutton: TPanel
        Left = 76
      end
    end
  end
  inline FRAMEfindConsigneeMarket: TFRAMEFindEditSOA [4]
    Left = 85
    Top = 8
    Width = 495
    Height = 21
    HorzScrollBar.Visible = False
    VertScrollBar.Visible = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
```
<!-- tabs:end -->

