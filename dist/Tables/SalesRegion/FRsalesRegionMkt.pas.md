<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRsalesRegionMkt` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRsalesRegionMkt` code unit defines a Delphi frame (`TFRAMEsalesRegionMkt`) that manages a grid-based interface for handling sales regions and their associated markets. It provides functionality for adding, deleting, and editing market data within a sales region. The frame is designed to interact with a database and SOAP services to manage and display data dynamically.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services to fetch or update data.
- **Database Components**: For managing and displaying data from a database.
- **cxGrid**: A grid component from DevExpress for displaying tabular data.
- **Custom Components**: Includes custom field editors and utilities for managing grid behavior.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **marketCd**: String (Custom editor: `cxEDTfindMarket`).
2. **marketDesc**: String.

#### Grid Actions and Their Effects:
1. **Add**: Adds a new market to the sales region.
2. **Delete**: Removes a selected market from the sales region.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new market to the sales region.
2. Delete an existing market from the sales region.
3. Edit market details directly in the grid.
4. Search for a market using a custom editor (`cxEDTfindMarket`).

### Main Components:
- **Grid (`cxGrid`)**: Displays the list of markets.
- **Custom Editor (`cxEDTfindMarket`)**: Allows users to search for and add markets.
- **Action List**: Manages actions like "Add" and "Delete."

### Pseudo-code for Actions and Events:
- **Add Button Click**:  
  `if add button clicked then execute ACTaddExecute function`
- **Grid Cell Value Change**:  
  `if grid cell value changed then execute cxDBVtableEditValueChanged function`
- **Custom Editor Button Click**:  
  `if custom editor button clicked then execute m_SetFindMktAdd function`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized in the `Create` constructor.
   - Grid settings, available actions, and custom editors are configured.
   - Event handlers are assigned.

2. **User Interactions**:
   - Users can add or delete markets using the provided actions.
   - Users can edit market details directly in the grid.
   - Users can search for markets using the custom editor.

### Functions and File Locations:
- **`Create` Constructor**: Initializes the frame and its components (`FRsalesRegionMkt.pas`).
- **`ACTaddExecute`**: Handles the "Add" action (`FRsalesRegionMkt.pas`).
- **`cxDBVtableEditValueChanged`**: Handles grid cell value changes (`FRsalesRegionMkt.pas`).
- **`m_SetFindMktAdd`**: Handles custom editor button clicks (`FRsalesRegionMkt.pas`).

### Required Data:
- Sales region code (`salesRegionCd`).
- Market code and description.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Action**: Requires a valid market code to be selected.
- **Delete Action**: Requires a market to be selected in the grid.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **marketCd**: Custom editor is used for validation.
- **marketDesc**: No specific validation is defined.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame and configures grid settings, actions, and event handlers.

2. **`ACTaddExecute`**:
   - Adds a new market to the sales region.

3. **`cxDBVtableEditValueChanged`**:
   - Handles changes to grid cell values.

4. **`m_SetFindMktAdd`**:
   - Handles the custom editor button click to search for and add a market.

---

## 6. API Service Consumption:

- **Service Name**: `CustomerMarketServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Market details (e.g., `marketCd`, `marketDesc`).
- **Data Received**: Confirmation of the operation.
- **Purpose**: To fetch or update market data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Custom Editor (`cxEDTfindMarket`)**:
  - Appears when the user interacts with the grid to add a market.

---

## 8. Dependencies:

### External Libraries:
- **DevExpress Components**: For grid and editor functionality.
- **SOAPHTTPClient**: For SOAP service interactions.

### Custom Components:
- **`kneFRGridEditSOA`**: Base class for the frame.
- **`CustomerMarketsToAddRegionServiceUtils`**: Utility for interacting with market-related services.

---

## 9. Fields and Validations Listing:

1. **marketCd**:
   - Type: String.
   - Required: Yes.
   - Custom Editor: `cxEDTfindMarket`.

2. **marketDesc**:
   - Type: String.
   - Required: No.

### Mapping of Displayed Values and Database Columns:
- **marketCd**: Maps to the `marketCd` field in the database.
- **marketDesc**: Maps to the `marketDesc` field in the database.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Data] --> [User Interaction]
    --> [Add/Delete/Edit Market] --> [Update Database] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame: Add/Delete/Edit Market
Frame --> SOAP Service: Fetch/Update Data
SOAP Service --> Frame: Response
Frame --> User: Update Grid
```

### Code Snippets:
```delphi
procedure TFRAMEsalesRegionMkt.ACTaddExecute(Sender: TObject);
begin
  // Logic to add a new market
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`Create` Constructor**:
  - Configures grid settings and available actions.
- **`m_SetFindMktAdd`**:
  - Handles custom editor button clicks for market search.

---

## 12. Conclusion:

The `FRsalesRegionMkt` code unit provides a robust framework for managing sales regions and their associated markets. It leverages DevExpress components and SOAP services for a dynamic and interactive user experience. However, the code lacks explicit error handling and validation logic, which could be improved.

---

## 13. Short Summary:

The `FRsalesRegionMkt` unit manages a grid-based interface for sales regions and markets, supporting add, delete, and edit actions. It integrates with SOAP services and custom components for dynamic data handling.#### **FRsalesRegionMkt.pas**

```
unit FRsalesRegionMkt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  CustomerMarketsToAddRegionServiceUtils;

type
  TFRAMEsalesRegionMkt = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }

    mv_AlreadyAdded: String;

    procedure m_OnSetAccessMode(Sender: TObject; var pv_value: Boolean);
    function SetFieldRequiredState(const pv_Field: TField;
      pv_State: Boolean): Boolean;
    procedure m_SetFindMktAdd(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
      function m_AlreadyAdded: string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEsalesRegionMkt: TFRAMEsalesRegionMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils, kneFGDBUtils,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMEsalesRegionMkt.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'salesRegionCd';
  DataPacketName := 'SalesRegionMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'salesRegionMkts';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'add;delete';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    DefineHiddenFields('HIDE_ALL_FIELDS');
    DefineOrderFields('marketCd;marketDesc');

    // Key Fields ..............................................................
//    KeyFields:= '';

    // Custom Editors ..........................................................
    AddCustomField('marketCd','cxEDTfindMarket');
    UseColsBestFit := False;
  end; //with

  OnSetAccessMode := m_OnSetAccessMode;
  cxEDTfindMarket.Properties.OnButtonClick := m_SetFindMktAdd;

  mv_AlreadyAdded := '';

end;

procedure TFRAMEsalesRegionMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

  SetColsWidthInGrid(gc_ColsWith, cxDBVtable);

  if (not assigned(CDStable)) or (not CDStable.Active) then
    exit;

//  CDStable.FieldByName('marketCd').ReadOnly := False;
```

#### **FRsalesRegionMkt.dfm**

```
inherited FRAMEsalesRegionMkt: TFRAMEsalesRegionMkt
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindMarket: TcxEditRepositoryButtonItem
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
    end
  end
end
```
<!-- tabs:end -->

