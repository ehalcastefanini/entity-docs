<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcarrierAvailableVehicle`

## 1. Overview:

### Objective and Problem Solved:
The `FRcarrierAvailableVehicle` unit is designed to manage and display a grid of available vehicles for a carrier. It provides functionalities to add, delete, and edit vehicle details, as well as search for specific vehicle types. This component is part of a larger system that handles carrier and vehicle management, streamlining the process of managing vehicle data.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP Services**: The code interacts with SOAP-based web services for vehicle-related operations.
- **Database Components**: Uses `TcxGridDBTableView` for displaying and managing data from a database.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **vehicleType**: String (Editable, with a custom button for searching).
2. **description**: String (Read-only).
3. **numVehicle**: Integer (Read-only).
4. **box**: Boolean (Checkbox).

#### Grid Actions and Their Effects:
1. **Add**: Allows the user to add a new vehicle.
2. **Delete**: Deletes the selected vehicle from the grid.

---

## 2. Functionality Description:

### User/Software Actions:
- Add a new vehicle.
- Delete an existing vehicle.
- Search for a vehicle type using a custom button.
- Edit specific fields in the grid.

### Main Components:
1. **Grid (`cxDBVtable`)**: Displays vehicle data.
2. **Custom Editors**:
   - `cxEDTfindVehicleType`: Button for searching vehicle types.
   - `cxEDTRCheckBox`: Checkbox for the "box" field.
3. **Action Panel**: Provides buttons for adding and deleting vehicles.

### Pseudo-code for Actions and Events:
- **OnClick event of Add button**: `if Add button clicked then execute ACTaddExecute`.
- **OnClick event of Delete button**: `if Delete button clicked then delete selected row`.
- **OnButtonClick event of vehicleType field**: `if button clicked then execute m_FindVehicleType`.
- **OnEditValueChanged event of grid**: `if grid cell value changed then execute cxDBVtableEditValueChanged`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEcarrierAvailableVehicle` frame is created.
   - Grid settings are configured (e.g., hidden fields, order of fields, key fields, custom editors).
   - Action panel is set up with "Add" and "Delete" actions.
2. **User Interactions**:
   - Users can add or delete vehicles using the action panel.
   - Users can search for vehicle types using the custom button in the `vehicleType` column.
   - Users can edit specific fields in the grid.

### Data Input:
- **vehicleType**: Editable, with a search button.
- **box**: Editable, as a checkbox.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**:
   - Preconditions: None.
   - Action: Adds a new row to the grid.
2. **Delete**:
   - Preconditions: A row must be selected.
   - Action: Deletes the selected row.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- **box**: Default value is "N" (unchecked).

### Field Validation and Conditions:
- **vehicleType**: Must be a valid vehicle type (validated via `m_FindVehicleType`).
- **box**: Accepts only "Y" (checked) or "N" (unchecked).

---

## 5. Main Functions:

1. **`Create`**:
   - Configures the grid and action panel.
   - Sets up custom editors and event handlers.
2. **`m_FindVehicleType`**:
   - Handles the search functionality for the `vehicleType` field.
3. **`m_FindByCodeVehicleType`**:
   - Validates and retrieves details for a specific vehicle type.
4. **`cxDBVtableEditValueChanged`**:
   - Handles changes in grid cell values.
5. **`ACTaddExecute`**:
   - Adds a new vehicle to the grid.

---

## 6. API Service Consumption:

- **Service Name**: `VehicleServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Vehicle type or other related data for validation or retrieval.
- **Data Received**: Vehicle details.
- **Purpose**: Validate and retrieve vehicle details.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Field**: `vehicleType`.
- **Condition**: The search button is only functional when the field is not empty.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Components**: For interacting with web services.
- **DevExpress TcxGrid**: For grid display and data management.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`**: Base frame for grid editing.
- **`TVehicleServiceUtils`**: Utility class for vehicle-related operations.

---

## 9. Fields and Validations Listing:

1. **vehicleType**:
   - Type: String.
   - Editable: Yes.
   - Validation: Must be a valid vehicle type.
2. **description**:
   - Type: String.
   - Editable: No.
3. **numVehicle**:
   - Type: Integer.
   - Editable: No.
4. **box**:
   - Type: Boolean.
   - Editable: Yes.
   - Default: "N" (unchecked).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcarrierAvailableVehicle.m_FindVehicleType(Sender: TObject; AButtonIndex: Integer);
begin
  // Logic to search for a vehicle type
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Hidden fields: `carrierCode`.
  - Key fields: `carrierCode;vehicleType`.
  - Custom editors: `vehicleType` (button), `box` (checkbox).
- **Action Panel**:
  - Available actions: "ADD;DELETE".

---

## 12. Conclusion:

The `FRcarrierAvailableVehicle` unit provides a robust framework for managing carrier vehicles. It integrates grid-based data management with custom search and validation functionalities. However, the lack of explicit error handling and detailed API documentation could be improved.

---

## 13. Short Summary:

The `FRcarrierAvailableVehicle` unit manages a grid of carrier vehicles, allowing users to add, delete, and search for vehicles. It integrates SOAP services for validation and retrieval, with customizable grid settings and action panels.#### **FRcarrierAvailableVehicle.pas**

```
unit FRcarrierAvailableVehicle;

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
  TFRAMEcarrierAvailableVehicle = class(TFRAMEBaseGridEditSOA)
    cxEDTfindVehicleType: TcxEditRepositoryButtonItem;
    cxEDTRCheckBox: TcxEditRepositoryCheckBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindVehicleType(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeVehicleType(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEcarrierAvailableVehicle: TFRAMEcarrierAvailableVehicle;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  VehicleServiceUtils, kneTypes, kneFindDialog, kneDialogFactory,
  kneFREditSOA;

{$R *.dfm}

constructor TFRAMEcarrierAvailableVehicle.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'code=carrierCode';
  DataPacketName := 'Vehicle';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'vehicles';          // nome do campo da metadata que vai conter os details
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
    HiddenFields.Add('carrierCode');
    // Ordem Campos ............................................................
    OrderFields.Add('vehicleType');
    OrderFields.Add('description');
    OrderFields.Add('numVehicle');
    OrderFields.Add('box');
    // Key Fields ..............................................................
    KeyFields:= 'carrierCode;vehicleType';
    // Custom Editors ..........................................................
    AddCustomField('vehicleType','cxEDTfindVehicleType');
    AddCustomField('box','cxEDTRCheckBox');
  end; //with

  // Atribui��o dos eventos dos Finds  
  cxEDTfindVehicleType.Properties.OnButtonClick := m_FindVehicleType;
end;

procedure TFRAMEcarrierAvailableVehicle.m_FindByCodeVehicleType(
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
  try
    lv_Column := cxDBVtable.GetColumnByFieldName('vehicleType');
    if (lv_Column <> nil) then
    begin
       lv_Key :=
```

#### **FRcarrierAvailableVehicle.dfm**

```
inherited FRAMEcarrierAvailableVehicle: TFRAMEcarrierAvailableVehicle
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindVehicleType: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
    object cxEDTRCheckBox: TcxEditRepositoryCheckBoxItem
      Properties.NullStyle = nssUnchecked
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
  end
end
```
<!-- tabs:end -->

