<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsVehType` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRconsVehType` code unit defines a frame (`TFRAMEconsVehType`) that provides a grid-based interface for managing vehicle types associated with consignees. It allows users to add, edit, and delete vehicle types, as well as configure specific properties such as default vehicle status. The frame is designed to handle data interactions and user actions efficiently, ensuring proper validation and data integrity.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., `VehicleServiceUtils`).
- **Database Components**: For managing data (`TClientDataSet`, `DB`).
- **DevExpress Components**: For advanced grid and editor functionalities (`cxGrid`, `cxEditRepository`).

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **vehTypeCode**: String (Editable, Custom Editor: `cxEDTfind`).
2. **vehType**: String (Editable).
3. **defaultVehicle**: Boolean (Checkbox, Custom Editor: `cxEDTdefaultVehicle`).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new vehicle type record.
2. **Delete**: Deletes the selected vehicle type record.
3. **Edit**: Allows editing of existing records in the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new vehicle type.
2. Edit existing vehicle type details.
3. Delete a vehicle type.
4. Search for vehicle types using a custom search dialog.
5. Mark a vehicle type as the default.

### Main Components:
- **Grid (`cxGrid`)**: Displays vehicle type data.
- **Custom Editors**: Includes a checkbox for default vehicle and a search button for vehicle type codes.
- **Action Panel**: Provides buttons for adding and deleting records.

### Pseudo-Code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changed then validate and update the record`.
- **OnNewRecord**: `if new record created then initialize default values`.
- **OnAddButtonClick**: `if add button clicked then create a new record`.
- **OnSearchButtonClick**: `if search button clicked then open search dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific settings (e.g., hidden fields, key fields, custom editors).
   - The action panel is configured to show "Add" and "Delete" actions.
2. **User Interaction**:
   - Users interact with the grid to add, edit, or delete records.
   - Custom editors (e.g., search button, checkbox) provide additional functionality.
3. **Event Handling**:
   - Events like `OnEditValueChanged` and `OnNewRecord` handle data validation and initialization.

### Data Requirements:
- **vehTypeCode**: Must be unique and valid.
- **vehType**: Descriptive name of the vehicle type.
- **defaultVehicle**: Boolean indicating if the vehicle type is the default.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**: Enabled at all times.
2. **Delete**: Enabled only when a record is selected.
3. **Edit**: Automatically triggered when a cell value is changed.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Value cannot be empty" if a required field is left blank.
- "Invalid vehicle type code" if the code does not exist in the system.

### Default Field Values:
- **defaultVehicle**: Default is unchecked (`N`).

### Field Validation and Conditions:
- **vehTypeCode**: Must be unique and non-empty.
- **vehType**: Must be non-empty.
- **defaultVehicle**: Checkbox with values `Y` (checked) and `N` (unchecked).

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame with default settings.
   - Configures grid properties and event handlers.

2. **`m_FindVehType`**:
   - Opens a search dialog to find and select a vehicle type.

3. **`m_FindByCodeVehicleType`**:
   - Validates and retrieves vehicle type details based on the entered code.

4. **`ACTaddExecute`**:
   - Adds a new record to the dataset.

---

## 6. API Service Consumption:

- **Service Name**: `VehicleServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Vehicle type code and other related fields.
- **Data Received**: Vehicle type details.
- **Purpose**: Validate and retrieve vehicle type information.
- **Error Handling**: If the service call fails, the operation is aborted.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `defaultVehicle`.
- **Condition**: Always visible and editable.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress Components**: For grid and editor functionalities.
- **SOAP Components**: For service interactions.

### Custom Components:
- **`kneFRGridEditSOA`**: Base frame for grid editing.
- **`kneFindDialogSOA`**: Custom search dialog.

---

## 9. Fields and Validations Listing:

1. **vehTypeCode**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be unique and non-empty.

2. **vehType**:
   - Type: String.
   - Required: Yes.
   - Validation: Must be non-empty.

3. **defaultVehicle**:
   - Type: Boolean.
   - Required: No.
   - Validation: Checkbox with values `Y` and `N`.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Data] --> [User Interaction]
    --> [Add/Edit/Delete Record] --> [Validate Data] --> [Save Changes] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame: Add/Edit/Delete
Frame --> SOAP Service: Validate/Fetch Data
SOAP Service --> Frame: Return Data
Frame --> User: Display Updated Data
```

### Code Snippets:
```delphi
procedure TFRAMEconsVehType.ACTaddExecute(Sender: TObject);
begin
  CDStable.Append; // Add a new record
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Initialization**:
  - `MasterKeyFields`, `DataPacketName`, and `PropertyName` are set to define the frame's data context.
- **Grid Settings**:
  - Hidden fields: `consCode`, `updBy`, `lastUpd`.
  - Key fields: `consCode`, `vehTypeCode`.

---

## 12. Conclusion:

The `FRconsVehType` code unit provides a robust and flexible grid-based interface for managing vehicle types. It integrates seamlessly with external services and ensures data integrity through validation and event handling. However, the lack of explicit error handling for service failures could be improved.

---

## 13. Short Summary:

The `FRconsVehType` unit defines a grid-based interface for managing vehicle types, supporting add, edit, and delete actions with validation and SOAP service integration. It is a reusable and configurable component for consignee vehicle type management.#### **FRconsVehType.pas**

```
unit FRconsVehType;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEconsVehType = class(TFRAMEBaseGridEditSOA)
    cxEDTdefaultVehicle: TcxEditRepositoryCheckBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindVehType(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeVehicleType(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEconsVehType: TFRAMEconsVehType;

implementation

uses
  kneInterfaces, kneUtils, kneFindDialogSOA, BaseServiceUtils, Global,
  VehicleServiceUtils, kneTypes,kneFindDialog,kneDialogFactory,
  kneConfigObjects;

{$R *.dfm}

{ TFRAMEconsVehType }

constructor TFRAMEconsVehType.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode';
  DataPacketName := 'ConsigneeVehType';
  PropertyName := 'consigneeVehTypes';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('consCode');
    HiddenFields.Add('updBy');
    HiddenFields.Add('lastUpd');
    // Ordem Campos ............................................................
    OrderFields.Add('vehTypeCode');
    OrderFields.Add('vehType');
    OrderFields.Add('defaultVehicle');
    // Key Fields ..............................................................
    KeyFields:= 'consCode;vehTypeCode';
    // Custom Editors ..........................................................
    AddCustomField('vehTypeCode','cxEDTfind');
    AddCustomField('defaultVehicle','cxEDTdefaultVehicle');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindVehType;
  //@@@@@
  CDStable.Tag := 6;
  DStable.Tag := 6;
end;

procedure TFRAMEconsVehType.m_FindByCodeVehicleType(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TVehicleServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
  lv_Key: string;
  lv_Cursor: TCursor;
  lv_Result: Boolean;
begin
  if Sender.Controller.EditingController.Edit.EditValue = '' then
    Exit;
  lv_Service := nil;
  lv_TargetDescFields := nil;
  lv_DescFields := nil;
```

#### **FRconsVehType.dfm**

```
inherited FRAMEconsVehType: TFRAMEconsVehType
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTdefaultVehicle: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
  end
end
```
<!-- tabs:end -->

