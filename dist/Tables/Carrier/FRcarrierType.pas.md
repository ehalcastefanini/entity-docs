<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcarrierType` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcarrierType` code unit defines a Delphi frame (`TFRAMEcarrierType`) that extends a base grid-editing frame (`TFRAMEBaseGridEditSOA`). Its primary purpose is to manage and display carrier types in a grid format, allowing users to view, add, and edit carrier type data. The frame integrates with a SOAP-based service (`CarrierTypeServiceUtils`) to fetch and manipulate carrier type data.

This frame is part of a larger system that likely manages logistics or transportation-related data, where carrier types are a key entity.

### Technologies Used:
- **Delphi VCL Framework**: For UI components and event handling.
- **SOAP Services**: For interacting with external services (`CarrierTypeServiceUtils`).
- **Database Components**: For binding data to the grid (`cxDBData`, `DBClient`).
- **DevExpress Grid Components**: For advanced grid functionalities (`cxGrid`, `cxGridDBTableView`).

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **status**: Displays the status of the carrier type (e.g., active/inactive).
2. **carrierType**: Displays the carrier type code.
3. **carrierTypeDesc**: Displays the description of the carrier type.
4. **lastUpd**: Displays the last update timestamp.
5. **updBy**: Displays the user who last updated the record.

#### Grid Actions and Their Effects:
1. **Add**: Allows the user to add a new carrier type.
2. **Edit**: Allows the user to edit existing carrier type data.
3. **Find**: Provides search functionality for carrier types.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add Carrier Type**: Users can add a new carrier type using the "Add" action.
2. **Edit Carrier Type**: Users can modify existing carrier type data.
3. **Search Carrier Type**: Users can search for carrier types using the "Find" functionality.

### Main Components:
1. **Grid (`cxGrid`)**: Displays carrier type data.
2. **Action Panel**: Provides buttons for actions like "Add."
3. **Custom Editors**: Includes custom field editors for specific columns (e.g., `cxEDTfind` for searching).

### Pseudo-Code for Actions and Events:
- `OnEditValueChanged` event of the grid:
  - `if grid cell value changed then execute validation or update logic`.
- `OnButtonClick` event of the search editor:
  - `if search button clicked then open search dialog`.
- `OnExecute` event of the "Add" action:
  - `if add button clicked then open add carrier type dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific settings for the grid, such as hidden fields, column order, and custom editors.
   - The `Create` constructor sets up the frame's metadata and grid configuration.
2. **User Interaction**:
   - Users interact with the grid to view, edit, or add carrier types.
   - Clicking the "Add" button triggers the `ACTaddExecute` method.
   - Editing a grid cell triggers the `cxDBVtableEditValueChanged` method.
3. **Service Interaction**:
   - The `m_FindByCodeCarrierType` method interacts with the `CarrierTypeServiceUtils` to fetch carrier type data.

### Data Requirements:
- Users must provide carrier type details (e.g., code, description) when adding or editing a record.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add Action**:
   - Preconditions: None.
   - Action: Opens a dialog to add a new carrier type.
2. **Edit Action**:
   - Preconditions: A record must be selected in the grid.
   - Action: Allows editing of the selected record.
3. **Find Action**:
   - Preconditions: None.
   - Action: Opens a search dialog.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **carrierType**: Custom editor (`cxEDTfind`) is used for searching.
- **status**: Custom editor (`cxEDTstat`) is used for status display.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame and configures the grid settings.
2. **`SetKeyEditing`**:
   - Disables editing for specific fields in the grid.
3. **`m_FindByCodeCarrierType`**:
   - Searches for a carrier type by code using the `CarrierTypeServiceUtils`.

---

## 6. API Service Consumption:

### Service Name: `CarrierTypeServiceUtils`
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Carrier type code or other search parameters.
- **Data Received**: Carrier type details.
- **Purpose**: Fetch carrier type data for display or validation.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: For grid and editor functionalities.
2. **SOAP Components**: For interacting with external services.

### Custom Components:
1. **`TFRAMEBaseGridEditSOA`**: Base class for the frame.
2. **`CarrierTypeServiceUtils`**: Utility class for interacting with the carrier type service.

---

## 9. Fields and Validations Listing:

1. **status**: (type: string, read-only).
2. **carrierType**: (type: string, editable, custom editor: `cxEDTfind`).
3. **carrierTypeDesc**: (type: string, editable).
4. **lastUpd**: (type: datetime, read-only).
5. **updBy**: (type: string, read-only).

Mapping of displayed values to database columns is not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcarrierType.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new carrier type
end;
```

### Screenshots:
The DFM file is not provided, so HTML representation is not applicable.

---

## 11. Important Comments in the Code:

1. **Grid Settings**:
   - Hidden fields: `carrier`.
   - Key fields: `carrier;carrierType`.
   - Custom editors: `carrierType` (search), `status` (status display).
2. **Action Panel**:
   - Visibility: Enabled.
   - Available actions: "ADD."

---

## 12. Conclusion:

The `FRcarrierType` code unit provides a robust framework for managing carrier types in a grid-based interface. It integrates with external services for data retrieval and supports advanced grid functionalities. However, the code lacks explicit error handling and detailed documentation for API interactions.

---

## 13. Short Summary:

The `FRcarrierType` unit defines a grid-based interface for managing carrier types, integrating with SOAP services for data retrieval. It supports adding, editing, and searching carrier types, with customizable grid settings and advanced editor functionalities.#### **FRcarrierType.pas**

```
unit FRcarrierType;

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
  TFRAMEcarrierType = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    FAddedTypes: string;
    procedure m_FindByCodeCarrierType(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindCarrierType(Sender: TObject; AButtonIndex: Integer);
    procedure SetAddedTypes(const Value: string);
    procedure SetKeyEditing(const EditKey: Boolean);  override;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    property AddedTypes: string read FAddedTypes write SetAddedTypes;
  end;

var
  FRAMEcarrierType: TFRAMEcarrierType;

implementation

uses
  kneConfigObjects, kneTypes, kneFindDialog, kneDialogFactory, kneFGFindUtils,
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  //---
  CarrierTypeServiceUtils;

{$R *.dfm}

constructor TFRAMEcarrierType.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'code=carrier';
  DataPacketName := 'CarrierType';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'carrType';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('carrier');
    // Ordem Campos ............................................................
    DefineOrderFields('status; carrierType; carrierTypeDesc; lastUpd; updBy');
    // Key Fields ..............................................................
    KeyFields:= 'carrier;carrierType';
    // Custom Editors ..........................................................
    AddCustomField('carrierType','cxEDTfind');
    AddCustomField('status','cxEDTstat');
  end; //with
  ColsWidthInGrid := '100;100;200;80;80';
  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindCarrierType;
  FAddedTypes := '';
end;

// ################ SetKeyEditing  ###############################################
procedure TFRAMEcarrierType.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  SetNoEdittingInGridFields('status; carrierType', self);
end;


procedure TFRAMEcarrierType.m_FindByCodeCarrierType(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TCarrierTypeServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
  lv_Key: string;
  lv_Cursor: TCursor;
  lv_Result: Boolean;
begin
  if Sender.Controller.EditingController.Edit.EditValue = '' then
```

#### **FRcarrierType.dfm**

```
inherited FRAMEcarrierType: TFRAMEcarrierType
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
  end
end
```
<!-- tabs:end -->

