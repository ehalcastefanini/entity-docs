<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRwarehouseServices` Code Unit

---

## 1. Overview:

### Objective:
The `FRwarehouseServices` code unit is designed to manage and display warehouse service-related data in a grid format. It provides functionalities for interacting with service data, such as adding, deleting, and searching for services, carriers, destinations, shipping methods, and vehicles. The main objective is to facilitate the management of warehouse services through a user-friendly interface.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP**: SOAP-based HTTP client is used for service communication.
- **Database Components**: Includes database-related components like `DB`, `DBClient`, and `cxDBData`.
- **DevExpress Components**: Utilizes `cxGrid`, `cxEditRepositoryItems`, and other DevExpress components for grid and UI functionalities.

### Form Type:
This code represents a **grid display**.

#### Grid Columns and Their Types:
1. **serviceType**: ComboBox (Dropdown list).
2. **applyServico**: ComboBox (Dropdown list).
3. **serviceCode**: String.
4. **name**: String.
5. **entityCode**: String.
6. **entityDesc**: String.
7. **shipMethod**: String.
8. **shipMethodDesc**: String.
9. **vehicleType**: String.
10. **vehicleTypeDesc**: String.
11. **start**: String.
12. **startDesc**: String.
13. **destination**: String.
14. **destinationDesc**: String.
15. **contract**: String.

#### Grid Actions and Their Effects:
- **Add**: Adds a new service entry.
- **Delete**: Deletes the selected service entry.
- **Search**: Allows searching for services, carriers, destinations, shipping methods, and vehicles.

---

## 2. Functionality Description:

### User/Software Actions:
- Add a new service.
- Delete an existing service.
- Search for services, carriers, destinations, shipping methods, and vehicles.
- Edit service details in the grid.

### Main Components:
1. **Grid (`cxGrid`)**: Displays the list of services.
2. **Edit Repository Items (`cxEditRepository`)**: Provides dropdowns and buttons for user interaction.
3. **Action Panel**: Contains buttons for adding and deleting services.

### Pseudo-code for Actions and Events:
- **OnEditValueChanged**: `if grid cell value changed then execute validation or update logic`.
- **Add Button Click**: `if add button clicked then open form to add new service`.
- **Delete Button Click**: `if delete button clicked then remove selected service`.
- **Search Button Click**: `if search button clicked then open search dialog`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEwarehouseServices` frame is created.
   - Grid settings, hidden fields, and available actions are configured.
   - The action panel is displayed.

2. **User Interactions**:
   - Users can add, delete, or search for services using the action panel.
   - Editing a grid cell triggers the `OnEditValueChanged` event.

3. **Functions**:
   - **File**: `FRwarehouseServices.pas`
   - **Functions**:
     - `m_FindService`: Handles service search logic.
     - `m_FindServiceCarrier`: Handles carrier search logic.
     - `m_FindServiceDestinationD`: Handles destination search logic.
     - `m_FindServiceShipMethod`: Handles shipping method search logic.
     - `m_FindServiceVehicle`: Handles vehicle search logic.

### Data Input:
- Users must provide service details such as type, code, name, and associated entities.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add**: Enabled at all times.
- **Delete**: Enabled only when a service is selected in the grid.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

1. **`m_FindService`**: Searches for a service based on user input.
2. **`m_FindServiceCarrier`**: Searches for a carrier.
3. **`m_FindServiceDestinationD`**: Searches for a destination.
4. **`m_FindServiceShipMethod`**: Searches for a shipping method.
5. **`m_FindServiceVehicle`**: Searches for a vehicle.

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined.
- **Endpoint**: Not explicitly defined.
- **Data Sent**: Not explicitly defined.
- **Data Received**: Not explicitly defined.
- **Purpose**: Not explicitly defined.
- **Error Handling**: Not explicitly defined.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress Components**: Used for grid and UI functionalities.
- **SOAPHTTPClient**: Used for SOAP-based service communication.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base class for the frame.

---

## 9. Fields and Validations Listing:

### Fields:
1. **serviceType**: ComboBox, required.
2. **applyServico**: ComboBox, required.
3. **serviceCode**: String, required.
4. **name**: String, required.
5. **entityCode**: String, required.
6. **entityDesc**: String, optional.
7. **shipMethod**: String, optional.
8. **shipMethodDesc**: String, optional.
9. **vehicleType**: String, optional.
10. **vehicleTypeDesc**: String, optional.
11. **start**: String, optional.
12. **startDesc**: String, optional.
13. **destination**: String, optional.
14. **destinationDesc**: String, optional.
15. **contract**: String, optional.

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEwarehouseServices.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new service
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Fields**: Defined in the constant `mc_GRID_FIELDS`.
- **Master Key Fields**: Set to `warehouseCode`.
- **Data Packet Name**: Set to `Service`.

---

## 12. Conclusion:

The `FRwarehouseServices` code unit provides a robust framework for managing warehouse services. It leverages Delphi's VCL and DevExpress components to create a user-friendly grid interface. However, the code lacks explicit error handling, field validations, and API integration details.

---

## 13. Short Summary:

The `FRwarehouseServices` code unit manages warehouse services through a grid interface, allowing users to add, delete, and search for services. It uses Delphi and DevExpress components for UI and SOAP for service communication.#### **FRwarehouseServices.pas**

```
unit FRwarehouseServices;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEwarehouseServices = class(TFRAMEBaseGridEditSOA)
    cxEDTserviceType: TcxEditRepositoryComboBoxItem;
    cxEDTfindServiceCarrier: TcxEditRepositoryButtonItem;
    cxEDTfindService: TcxEditRepositoryButtonItem;
    cxEDTfindServiceShipMethod: TcxEditRepositoryButtonItem;
    cxEDTfindServiceVehicle: TcxEditRepositoryButtonItem;
    cxEDTfindServiceDestinationS: TcxEditRepositoryButtonItem;
    cxEDTfindServiceDestinationD: TcxEditRepositoryButtonItem;
    cxEDTRapplyServico: TcxEditRepositoryComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindService(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceCarrier(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceDestinationD(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceDestinationS(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceShipMethod(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindServiceVehicle(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeService(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeCarrier(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeDestinationD(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeDestinationS(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeShipMethod(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeVehicle(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;
                                        
var
  FRAMEwarehouseServices: TFRAMEwarehouseServices;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, Global, BaseServiceUtils,
  kneTypes, kneFindDialog, kneDialogFactory, kneFGFindUtils,
  DestinationServiceUtils, ShippingServiceUtils,
  VehicleServiceUtils, ServiceServiceUtils, CarrierServiceUtils,
  FRfindCriteriaDestination, kneConfigObjects;

const
  mc_GRID_FIELDS = 'serviceType;applyServico;serviceCode;name;entityCode;' // [11-06-2015, #21987]
    + 'entityDesc;shipMethod;shipMethodDesc;vehicleType;vehicleTypeDesc;'
    + 'start;startDesc;destination;destinationDesc;contract';

{$R *.dfm}

constructor TFRAMEwarehouseServices.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Service'; // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'services';  // nome do campo da metadata que vai conter os details
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
//    HiddenFields.Add('warehouseCode');
//    HiddenFields.Add('updBy');
//    HiddenFields.Add('lastUpd');
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
//    OrderFields.Add('serviceType');
//    OrderFields.Add('serviceCode');
//    OrderFields.Add('name');
////    OrderFields.Add('serviceDesc');
```

#### **FRwarehouseServices.dfm**

```
inherited FRAMEwarehouseServices: TFRAMEwarehouseServices
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTserviceType: TcxEditRepositoryComboBoxItem
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        'PLAN'
        'PERFIL')
    end
    object cxEDTfindServiceCarrier: TcxEditRepositoryButtonItem
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
    object cxEDTfindService: TcxEditRepositoryButtonItem
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
    object cxEDTfindServiceShipMethod: TcxEditRepositoryButtonItem
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
```
<!-- tabs:end -->

