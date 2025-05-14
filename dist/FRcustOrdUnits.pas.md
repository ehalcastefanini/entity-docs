<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustOrdUnits`

## 1. Overview:

### Objective and Problem Solved:
The `FRcustOrdUnits` unit defines a Delphi frame (`TFRAMEcustOrdUnits`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display customer order units in a grid format. It provides functionalities for adding, editing, and managing customer order units, including custom field handling and integration with a find dialog for selecting units.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services.
- **Database Components**: For managing and displaying data in a grid.
- **Custom Components**: Includes custom grid settings and find dialog utilities.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **businessUnit**: String.
2. **formatOrc**: String.
3. **unitCd**: String (with custom editor for find functionality).
4. **unitDesc**: String.

#### Grid Actions and Their Effects:
1. **Add Button**: Adds a new customer order unit.
2. **Edit Value Change**: Updates the grid when a value is edited.
3. **Find Button**: Opens a dialog to find and select a unit.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new customer order unit using the "Add" button.
2. Edit existing values in the grid.
3. Use the find dialog to search and select a unit.

### Main Components:
- **Grid (`cxDBG`)**: Displays customer order units.
- **Add Button (`BTNadd`)**: Triggers the addition of a new unit.
- **Find Dialog (`TFORMkneFindDialog`)**: Allows users to search for units.
- **Custom Field (`unitCd`)**: Includes a custom editor for find functionality.

### Pseudo-code for Actions and Events:
- **OnClick event of Add Button**: `if add button clicked then execute add action`.
- **OnEditValueChanged event of Grid**: `if grid value changed then update corresponding data`.
- **OnButtonClick event of Find Button**: `if find button clicked then open find dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific settings for the grid and actions.
   - Custom field editors and event handlers are configured.
2. **User Interaction**:
   - Users can add, edit, or search for customer order units.
   - Clicking the "Find" button opens a dialog to search for units.
3. **Functions**:
   - `Create` (File: `FRcustOrdUnits.pas`): Initializes the frame and its components.
   - `m_SetFindUnitCd` (File: `FRcustOrdUnits.pas`): Handles the find dialog logic.
   - `ACTaddExecute` (File: `FRcustOrdUnits.pas`): Executes the add action.

### Required Data:
- **Customer Code (`custCd`)**: Used as a key field.
- **Unit Code (`unitCd`)**: Required for adding or editing units.
- **Unit Description (`unitDesc`)**: Optional but recommended for clarity.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Action: Adds a new row to the grid.
2. **Edit Action**:
   - Preconditions: A row must be selected.
   - Action: Updates the selected row.
3. **Find Action**:
   - Preconditions: None.
   - Action: Opens a dialog to search for units.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **unitCd**: Custom editor with find functionality.
- **unitDesc**: No explicit validation defined.

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and its components.
   - Configures grid settings and custom field editors.
2. **`m_SetFindUnitCd`**:
   - Opens the find dialog and maps selected values to the grid.
3. **`ACTaddExecute`**:
   - Adds a new customer order unit to the grid.

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined in the code.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely to fetch or update customer order units.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **unitCd**: Includes a custom editor with a find button.
- **Conditions**: The find dialog is triggered only when the find button is clicked.

---

## 8. Dependencies:

### External Libraries:
1. **SOAPHTTPClient**: For SOAP service integration.
2. **cxGrid**: For grid display and management.
3. **kneUtils**: Custom utility functions.

### Custom Components:
1. **TFORMkneFindDialog**: Custom dialog for finding units.
2. **TFRAMEBaseGridEditSOA**: Base frame for grid editing.

---

## 9. Fields and Validations Listing:

1. **businessUnit**: String, no explicit validation defined.
2. **formatOrc**: String, no explicit validation defined.
3. **unitCd**: String, custom editor with find functionality.
4. **unitDesc**: String, no explicit validation defined.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEcustOrdUnits.m_SetFindUnitCd(Sender: TObject; AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin
  lv_Find := TkneDialogFactory.GetFindDialog(Application);
  with lv_Find do
  begin
    Options.DataSelection.FieldNameForCode := 'priceUnitCode';
    Options.DataSelection.TargetFieldNameForCode := 'unitCd';
    ShowModal;
  end;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

1. **Grid Settings**:
   - `DefineHiddenFields('HIDE_ALL_FIELDS')`: Hides all fields initially.
   - `DefineOrderFields(mc_GRID_FIELDS)`: Orders fields based on the constant `mc_GRID_FIELDS`.

2. **Custom Field Editor**:
   - `AddCustomField('unitCd','cxEDTfind')`: Adds a custom editor for the `unitCd` field.

---

## 12. Conclusion:

The `FRcustOrdUnits` unit provides a robust framework for managing customer order units in a grid format. It includes custom field handling and integration with a find dialog. However, the code lacks explicit error handling, field validations, and API service details, which could limit its usability in certain scenarios.

---

## 13. Short Summary:

The `FRcustOrdUnits` unit defines a grid-based interface for managing customer order units, with features like custom field editors and a find dialog. It is part of a larger system for handling customer data but lacks explicit error handling and validation.#### **FRcustOrdUnits.pas**

```
unit FRcustOrdUnits;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneTypes;

type
  TFRAMEcustOrdUnits = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_SetFindUnitCd(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeUnitCd(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

  end;

var
  FRAMEcustOrdUnits: TFRAMEcustOrdUnits;

implementation

uses
  kneUtils, kneFindDialog, kneDialogFactory, kneConfigObjects, kneFGFindUtils,
  PriceUnitsByFormatsUtils;

const
  mc_GRID_FIELDS = 'businessUnit;formatOrc;unitCd;unitDesc';

{$R *.dfm}

constructor TFRAMEcustOrdUnits.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=custCd';
  DataPacketName := 'CustomerUnits';
  PropertyName := 'customerUnits';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
//    DefineReadOnlyFields('');
    DefineOrderFields(mc_GRID_FIELDS);

		// Custom Editors ..........................................................
    AddCustomField('unitCd','cxEDTfind');

  end; //with

  cxEDTfind.Properties.OnButtonClick := m_SetFindUnitCd;
end;


procedure TFRAMEcustOrdUnits.m_SetFindUnitCd(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find do
    begin
      with Options.DataSelection do
      begin
        FieldNameForCode := 'priceUnitCode';
        FieldNamesForDesc.Clear;
        FieldNamesForDesc.Add('descrip');

        TargetDataSet := cxDBVtable.DataController.DataSource.DataSet;
        TargetFieldNameForCode := 'unitCd';
        TargetFieldNamesForDesc.Clear;
        TargetFieldNamesForDesc.Add('unitDesc');

        UseTargetDataSet:= True;
```

#### **FRcustOrdUnits.dfm**

```
inherited FRAMEcustOrdUnits: TFRAMEcustOrdUnits
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
          Left = 2
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
```
<!-- tabs:end -->

