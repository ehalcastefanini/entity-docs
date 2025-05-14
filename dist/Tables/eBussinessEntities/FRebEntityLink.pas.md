<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRebEntityLink`

## 1. Overview:

### Objective and Problem Solved:
The `FRebEntityLink` unit defines a form (`TFRAMEebEntityLink`) that facilitates the management and linking of entities in a system. It provides a user interface for selecting and managing entity types, parties, and related metadata. The form is designed to handle entity relationships and metadata efficiently, ensuring proper data input and validation.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the user interface and handling events.
- **SOAP Services**: For interacting with external services (`EbEntityLinkServiceUtils`).
- **Database Components**: For managing and displaying data (`TDataSet`, `TClientDataSet`, `TsDBEdit`, `TcxDBImageComboBox`).
- **Custom Components**: Includes custom controls like `TFRAMEFindEditSOA` and `TFRAMEBaseCtrlEditSOA`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - Labels (`TsLabel`): Display field descriptions.
  - Combo Boxes (`TcxDBImageComboBox`): Dropdowns for selecting entity types and parties.
  - Text Fields (`TsDBEdit`): For entering or displaying entity URIs.
  - Custom Frame (`TFRAMEFindEditSOA`): For advanced entity search functionality.
- **Form Actions and Effects**:
  - Dropdown selection changes trigger specific logic (e.g., `ICBOentity_tpPropertiesEditValueChanged`).
  - Text field changes validate and process data.
  - Custom dialogs (`FindDlgAgent`, `FindDlgConsignee`, `FindDlgCustomer`) allow advanced search and selection.

---

## 2. Functionality Description:

### User/Software Actions:
- Select entity types and parties using dropdowns.
- Enter or view entity URIs in text fields.
- Use advanced search dialogs for finding entities.

### Main Components:
- **Labels (`TsLabel`)**: Provide field descriptions.
- **Combo Boxes (`TcxDBImageComboBox`)**: Allow selection of predefined options.
- **Text Fields (`TsDBEdit`)**: Enable data entry and display.
- **Custom Dialogs**: Facilitate advanced search and selection.

### Pseudo-code for Actions and Events:
- `OnChange` event of `ICBOentity_tp`:
  ```pseudo
  if dropdown value changes then
    execute ProcessEntityTpCombo function
  ```
- `OnScroll` event of `CDStable`:
  ```pseudo
  if dataset scrolls then
    execute CDStableAfterScroll function
  ```
- `OnClick` event of `ICBOentity_tp`:
  ```pseudo
  if dropdown clicked then
    execute ICBOentity_tpClick function
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized in the `Create` constructor.
   - Combos and dialogs are prepared (`m_PrepareFindDialogs`, `m_PrepareCombos`).
   - Default styles and properties are set.
2. **User Interaction**:
   - Users interact with dropdowns, text fields, and search dialogs.
   - Events like `OnChange` and `OnClick` trigger specific functions.
3. **Functions**:
   - `m_SetFindEntity`: Configures the search entity type.
   - `m_PrepareFindDialogs`: Prepares search dialogs.
   - `m_PrepareCombos`: Configures combo boxes.
   - `ProcessEntityTpCombo`: Handles logic for entity type selection.

### Data Input:
- Users must provide:
  - Entity type (via dropdown).
  - Party type (via dropdown).
  - Entity URI (via text field).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Dropdown Selection**:
  - Preconditions: Dropdown must be populated with valid options.
  - Action: Triggers logic to process the selected value.
- **Search Dialogs**:
  - Preconditions: Dialogs must be initialized.
  - Action: Allows advanced search and selection of entities.

### Available Filters:
- Entity Type: Dropdown options for entity types.
- Party Type: Dropdown options for party types.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`m_SetFindEntity`**:
   - Configures the search entity type based on the provided parameter.
2. **`m_PrepareFindDialogs`**:
   - Initializes and prepares search dialogs.
3. **`m_PrepareCombos`**:
   - Configures combo boxes with appropriate styles and data.
4. **`ProcessEntityTpCombo`**:
   - Handles logic when the entity type combo box value changes.

---

## 6. API Service Consumption:

### External Service Calls:
- **Service Name**: `EbEntityLinkServiceUtils`
- **Purpose**: Provides utility functions for managing entity links.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Entity URI Field**:
  - Always visible and editable.
- **Conditional Logic**:
  - Not explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components**: For service interaction.
- **Database Components**: For data management.
- **Custom Components**: `TFRAMEFindEditSOA`, `TFRAMEBaseCtrlEditSOA`.

### Custom Components:
- `TFRAMEFindEditSOA`: Advanced search functionality.
- `TFRAMEBaseCtrlEditSOA`: Base frame for editing.

---

## 9. Fields and Validations Listing:

### Fields:
1. **Entity Type**:
   - Type: Dropdown.
   - Validation: Not explicitly defined.
2. **Party Type**:
   - Type: Dropdown.
   - Validation: Not explicitly defined.
3. **Entity URI**:
   - Type: Text field.
   - Validation: Not explicitly defined.

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEebEntityLink.ICBOentity_tpPropertiesEditValueChanged(Sender: TObject);
begin
  ProcessEntityTpCombo(Sender);
end;
```

### Screenshots:
The following HTML represents the form layout:
```html
<div style="width: 616px; border: 1px solid #ccc; padding: 10px;">
  <div style="margin-bottom: 10px;">
    <label>Entity type:</label>
    <select></select>
  </div>
  <div style="margin-bottom: 10px;">
    <label>Entity:</label>
    <input type="text" />
  </div>
  <div style="margin-bottom: 10px;">
    <label>EbEntity type:</label>
    <select></select>
  </div>
  <div style="margin-bottom: 10px;">
    <label>EbParty Type:</label>
    <select></select>
  </div>
  <div style="margin-bottom: 10px;">
    <label>EbParty:</label>
    <input type="text" />
  </div>
  <div style="margin-bottom: 10px;">
    <label>Entity Uri:</label>
    <input type="text" />
  </div>
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization**:
  - Ensures pointers are set to `nil` to avoid memory issues.
- **Metadata Configuration**:
  - Sets `DataPacketName` and `FrameType` for proper metadata handling.

---

## 12. Conclusion:

The `FRebEntityLink` unit provides a robust framework for managing entity relationships and metadata. While it offers advanced search and selection capabilities, the lack of explicit error handling and validation logic may require additional implementation for production use.

---

## 13. Short Summary:

The `FRebEntityLink` unit defines a form for managing entity relationships, featuring dropdowns, text fields, and advanced search dialogs. It integrates with SOAP services and database components, providing a flexible and extensible solution for entity management.#### **FRebEntityLink.pas**

```
unit FRebEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, Mask, DBCtrls, sDBEdit, kneFRFindEditSOA, sLabel,
  kneFRStatusInfo, cxGraphics, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxDBEdit, kneFindDialog, DMskin;

type
  TFRAMEebEntityLink = class(TFRAMEBaseCtrlEditSOA)
    PNL2: TsPanel;
    LBLeb_party_tp: TsLabel;
    LBLeb_Party: TsLabel;
    FRAMEFindEntity: TFRAMEFindEditSOA;
    EDTeb_Party: TsDBEdit;
    ICBOentity_tp: TcxDBImageComboBox;
    LBLentity_tp: TsLabel;
    LBLentity_cd: TsLabel;
    LBLeb_entity_type: TsLabel;
    ICBOeb_entity_type: TcxDBImageComboBox;
    ICBOeb_Party_Tp: TcxDBImageComboBox;
    LBLentityUri: TsLabel;
    EDTentityUri: TsDBEdit;
    procedure ICBOentity_tpPropertiesEditValueChanged(Sender: TObject);
    procedure CDStableAfterScroll(DataSet: TDataSet);
    procedure ICBOentity_tpExit(Sender: TObject);
    procedure ICBOentity_tpClick(Sender: TObject);
  private

    FindDlgAgent : TFORMkneFindDialog;
    FindDlgConsignee : TFORMkneFindDialog;
    FindDlgCustomer : TFORMkneFindDialog;
    procedure m_SetFindEntity(const p_EntityType: string);
    procedure m_PrepareFindDialogs;
    procedure m_PrepareCombos;
    procedure m_DestroyFindDialogs;
    procedure m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure ProcessEntityTpCombo(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;

  end;

var
  FRAMEebEntityLink: TFRAMEebEntityLink;

implementation

uses
  kneUtils, kneTypes, kneFGFindUtils, kneDialogFactory, kneFGGenericUtils
  //--- ServiceUtils
  , EbEntityLinkServiceUtils, AgentServiceUtils, kneFREditSOA;


{$R *.dfm}

constructor TFRAMEebEntityLink.Create(AOwner: TComponent);
begin
  inherited;

  // para ter certesa que os ponteiros apontam para nada
  // issue com o FastMM
  FindDlgCustomer := nil;
  FindDlgConsignee := nil;
  FindDlgAgent := nil;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := '';
  DataPacketName := 'EbEntityLink';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := '';         // nome do campo da metadata(entidadeMaster) que vai conter os details
  FrameType := frtMaster;

  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  ProviderService := TEbEntityLinkServiceUtils.Create(Self);
  OnSetAccessMode := m_SetAccessMode;


  // Setup para os Finds
  m_PrepareFindDialogs;
  m_PrepareCombos;

  ICBOentity_tp.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_entity_type.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_Party_Tp.Style.StyleController := DMODskin.cxEditStyles1;
end;

destructor TFRAMEebEntityLink.Destroy;
begin
  m_DestroyFindDialogs;
  inherited;
```

#### **FRebEntityLink.dfm**

```
inherited FRAMEebEntityLink: TFRAMEebEntityLink
  Width = 616
  inherited PNLfooter: TsPanel
    Width = 616
  end
  object PNL2: TsPanel [1]
    Left = 0
    Top = 0
    Width = 616
    Height = 209
    Align = alTop
    TabOrder = 1
    SkinData.SkinSection = 'ALPHACOMBOBOX'
    object LBLeb_party_tp: TsLabel
      Left = 16
      Top = 112
      Width = 69
      Height = 13
      Caption = 'EbParty Type:'
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLeb_Party: TsLabel
      Left = 16
      Top = 144
      Width = 42
      Height = 13
      Caption = 'EbParty:'
      FocusControl = EDTeb_Party
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLentity_tp: TsLabel
      Left = 16
      Top = 16
      Width = 57
      Height = 13
      Caption = 'Entity type:'
      FocusControl = ICBOentity_tp
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLentity_cd: TsLabel
      Left = 16
      Top = 48
      Width = 32
      Height = 13
      Caption = 'Entity:'
      FocusControl = FRAMEFindEntity
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLeb_entity_type: TsLabel
      Left = 16
      Top = 80
      Width = 69
      Height = 13
      Caption = 'EbEntity type:'
      FocusControl = ICBOeb_entity_type
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLentityUri: TsLabel
      Left = 16
      Top = 176
      Width = 48
      Height = 13
      Caption = 'Entity Uri:'
      FocusControl = EDTentityUri
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    inline FRAMEFindEntity: TFRAMEFindEditSOA
      Left = 107
      Top = 43
      Width = 486
```
<!-- tabs:end -->

