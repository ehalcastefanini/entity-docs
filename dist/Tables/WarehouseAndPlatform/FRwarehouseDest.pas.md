<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRwarehouseDest` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRwarehouseDest` code unit is designed to manage and display a grid interface for warehouse destination data. It provides functionalities for adding, editing, and managing destination and shipping method information. The main objective is to allow users to interact with warehouse destination data efficiently, including searching for destinations and shipping methods using custom editors.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL framework for GUI development.
- **SOAP Services**: The code interacts with external services (`DestinationServiceUtils`, `ShippingServiceUtils`) for fetching and managing data.
- **Database Components**: Uses `DBClient` and `TDataSet` for database interactions.
- **cxGrid**: A component from DevExpress for displaying and managing grid data.
- **Custom Editors**: `TcxEditRepositoryButtonItem` for custom button-based editors.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **destinationCode**: String (Key Field).
2. **destinationDesc**: String.
3. **shipMethod**: String.
4. **shipMethodDesc**: String.
5. **sundayTransitTime**: Integer.
6. **mondayTransitTime**: Integer.
7. **tuesdayTransitTime**: Integer.
8. **wednesdayTransitTime**: Integer.
9. **thursdayTransitTime**: Integer.
10. **fridayTransitTime**: Integer.
11. **saturdayTransitTime**: Integer.
12. **freightCost**: Float.
13. **KMeter**: Float.
14. **stat**: String.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new destination record.
2. **Delete**: Deletes the selected destination record.
3. **Custom Editors**:
   - `cxEDTfindDestination`: Opens a dialog to find a destination.
   - `cxEDTfindDestinationShipMethod`: Opens a dialog to find a shipping method.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Add a Destination**: Users can add a new destination using the "Add" action.
2. **Delete a Destination**: Users can delete a selected destination using the "Delete" action.
3. **Search for Destination**: Users can search for a destination using the custom editor (`cxEDTfindDestination`).
4. **Search for Shipping Method**: Users can search for a shipping method using the custom editor (`cxEDTfindDestinationShipMethod`).

### Main Components:
1. **Grid (`cxGrid`)**: Displays the destination data.
2. **Custom Editors**: Provides search functionality for destinations and shipping methods.
3. **Action Panel**: Contains buttons for adding and deleting records.

### Pseudo-code for Actions and Events:
- **OnClick event of "Add" button**:  
  `if Add button clicked then execute ACTaddExecute function`.
- **OnClick event of "Delete" button**:  
  `if Delete button clicked then delete selected record`.
- **OnButtonClick event of `cxEDTfindDestination`**:  
  `if button clicked then execute m_FindDestination function`.
- **OnButtonClick event of `cxEDTfindDestinationShipMethod`**:  
  `if button clicked then execute m_FindDestinationShipMethod function`.
- **OnEditValueChanged event of grid**:  
  `if grid cell value changed then execute cxDBVtableEditValueChanged function`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEwarehouseDest` constructor initializes the grid settings, defines key fields, hidden fields, and custom editors.
   - Event handlers for custom editors are assigned.

2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` procedure to add a new record.
   - Clicking the "Delete" button deletes the selected record.
   - Clicking the custom editor buttons triggers the respective search functions (`m_FindDestination`, `m_FindDestinationShipMethod`).

### Data Input:
- Users must provide:
  - Destination Code.
  - Destination Description.
  - Shipping Method.
  - Transit Times (Sunday to Saturday).
  - Freight Cost.
  - Distance in Kilometers (KMeter).
  - Status (stat).

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**: Enabled at all times.
2. **Delete**: Enabled only when a record is selected.
3. **Search for Destination**: Requires clicking the custom editor button.
4. **Search for Shipping Method**: Requires clicking the custom editor button.

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

1. **`Create`**: Initializes the frame, sets grid properties, and assigns event handlers.
2. **`m_FindDestination`**: Handles the search for destinations.
3. **`m_FindDestinationShipMethod`**: Handles the search for shipping methods.
4. **`ACTaddExecute`**: Adds a new record to the grid.
5. **`cxDBVtableEditValueChanged`**: Handles changes in grid cell values.

---

## 6. API Service Consumption:

1. **Service Name**: `DestinationServiceUtils`.
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Fetch destination data.
2. **Service Name**: `ShippingServiceUtils`.
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Fetch shipping method data.

---

## 7. Conditional Fields (Form Logic):

- **Conditional Field**: None explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: Used for grid and custom editor functionalities.
2. **SOAPHTTPClient**: Used for SOAP service interactions.

### Custom Components:
1. **`kneFRGridEditSOA`**: Base class for grid editing.
2. **`kneFindDialogSOA`**: Used for search dialogs.

---

## 9. Fields and Validations Listing:

1. **destinationCode**: String, required.
2. **destinationDesc**: String, required.
3. **shipMethod**: String, required.
4. **shipMethodDesc**: String, optional.
5. **sundayTransitTime**: Integer, optional.
6. **mondayTransitTime**: Integer, optional.
7. **tuesdayTransitTime**: Integer, optional.
8. **wednesdayTransitTime**: Integer, optional.
9. **thursdayTransitTime**: Integer, optional.
10. **fridayTransitTime**: Integer, optional.
11. **saturdayTransitTime**: Integer, optional.
12. **freightCost**: Float, optional.
13. **KMeter**: Float, optional.
14. **stat**: String, optional.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEwarehouseDest.m_FindDestination(Sender: TObject; AButtonIndex: Integer);
begin
  // Logic to find a destination
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

1. **Grid Settings**:
   - Hidden fields: `'HIDE_ALL_FIELDS'`.
   - Key fields: `'warehouseCode;destinationCode'`.
   - Custom editors: `cxEDTfindDestination`, `cxEDTfindDestinationShipMethod`.

2. **Event Assignments**:
   - `cxEDTfindDestination.Properties.OnButtonClick := m_FindDestination`.

---

## 12. Conclusion:

The `FRwarehouseDest` code unit provides a robust grid interface for managing warehouse destination data. It integrates with external services for data retrieval and supports custom editors for enhanced user interaction. However, the code lacks explicit error handling, field validations, and default values.

---

## 13. Short Summary:

The `FRwarehouseDest` unit manages a grid interface for warehouse destinations, supporting add, delete, and search functionalities. It integrates with external services and uses custom editors for enhanced user interaction.#### **FRwarehouseDest.pas**

```
unit FRwarehouseDest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEwarehouseDest = class(TFRAMEBaseGridEditSOA)
    cxEDTfindDestination: TcxEditRepositoryButtonItem;
    cxEDTfindDestinationShipMethod: TcxEditRepositoryButtonItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableAfterInsert(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindDestination(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindDestinationShipMethod(Sender: TObject;AButtonIndex: Integer);

    procedure m_FindByCodeDestination(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeShipMethod(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;    
  end;

var
  FRAMEwarehouseDest: TFRAMEwarehouseDest;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  kneTypes, kneFindDialog, kneDialogFactory, kneFGFindUtils, kneFREditSOA,
  kneConfigObjects,
  //---
  DestinationServiceUtils, ShippingServiceUtils 
  ;

{$R *.dfm}

{ TFRAMEwarehouseDest }

constructor TFRAMEwarehouseDest.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'warehouseCode';
  DataPacketName := 'Destination';                       // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'destinations';                        // nome do campo da metadata que vai conter os details
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
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('destinationCode; destinationDesc; shipMethod; ' +
      'shipMethodDesc; sundayTransitTime; mondayTransitTime; tuesdayTransitTime;' +
      'wednesdayTransitTime; thursdayTransitTime; fridayTransitTime; ' +
      'saturdayTransitTime; freightCost; KMeter; stat');
    // Key Fields ..............................................................
    KeyFields:= 'warehouseCode;destinationCode';
    // Custom Editors ..........................................................
    AddCustomField('destinationCode','cxEDTfindDestination');
    AddCustomField('ShipMethod','cxEDTfindDestinationShipMethod');
    AddCustomField('stat','cxEDTstat');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfindDestination.Properties.OnButtonClick := m_FindDestination;
  cxEDTfindDestinationShipMethod.Properties.OnButtonClick := m_FindDestinationShipMethod;
end;

procedure TFRAMEwarehouseDest.m_FindByCodeDestination(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
var
  lv_Service: TDestinationServiceUtils;
  lv_TargetDescFields, lv_DescFields : TStringList;
  lv_Column: TcxGridDBColumn;
  lv_Key: string;
```

#### **FRwarehouseDest.dfm**

```
inherited FRAMEwarehouseDest: TFRAMEwarehouseDest
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindDestination: TcxEditRepositoryButtonItem
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
    object cxEDTfindDestinationShipMethod: TcxEditRepositoryButtonItem
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
  end
end
```
<!-- tabs:end -->

