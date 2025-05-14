<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRebEntityLinkOldValue`

## 1. Overview:

### Objective and Problem Solved:
The `FRebEntityLinkOldValue` code defines a Delphi frame (`TFRAMEebEntityLinkOldValue`) that inherits from `TFRAMEBaseCtrlEditSOA`. This frame is designed to manage and display details of an entity's old values in a master-detail relationship. The main objective is to prevent automatic updates of the detail data when the master data changes, ensuring that the old values remain static and unaltered.

### Technologies Used:
- **Delphi Framework**: The code is written in Delphi, utilizing its object-oriented programming capabilities.
- **SOAP Services**: The `SOAPHTTPClient` and `Rio` components suggest integration with SOAP-based web services.
- **Database Components**: The `DB` and `DBClient` units indicate interaction with databases.
- **UI Components**: Includes standard Delphi UI components like `Buttons`, `Panels`, and `Forms`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements**:
  - `MasterKeyFields` (string): Used to define the master-detail relationship fields.
  - `DataPacketName` (string): Specifies the name of the detail in the metadata.
  - `PropertyName` (string): Indicates the field in the master entity metadata containing the details.
  - `FrameType` (enum): Defines the type of frame (`frtDetail` in this case).
  - `ShowActionPanel` (boolean): Determines whether the action panel is visible.
  - `AvailableActions` (string): Specifies the actions available in the frame.
- **Form Actions**:
  - Prevents automatic updates of detail data when the master data changes.
  - Configures the frame to display static old values.

---

## 2. Functionality Description:

### User/Software Actions:
- The frame is initialized with specific properties to prevent automatic updates of detail data.
- It displays old values of an entity in a master-detail relationship.

### Main Components:
- **`MasterKeyFields`**: Ensures no fields are linked to avoid automatic updates.
- **`DataPacketName`**: Identifies the detail data in the metadata.
- **`PropertyName`**: Points to the field containing the details in the master entity.
- **`FrameType`**: Configures the frame as a detail frame.
- **`ShowActionPanel`**: Hides the action panel for this frame.
- **`AvailableActions`**: Disables all actions for this frame.

### Pseudo-code Translation:
- **Initialization**:
  ```pseudo
  if frame is created then
    set MasterKeyFields to empty
    set DataPacketName to 'EbEntityLink'
    set PropertyName to 'old'
    set FrameType to frtDetail
    hide ActionPanel
    disable AvailableActions
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created using the `Create` constructor.
   - Properties like `MasterKeyFields`, `DataPacketName`, `PropertyName`, `FrameType`, `ShowActionPanel`, and `AvailableActions` are set.
2. **User Interaction**:
   - No direct user interaction is expected as the frame is configured to display static data.

### Data Requirements:
- No user input is required. The frame relies on metadata and master entity data to display the old values.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Action**: Display old values of an entity.
  - **Precondition**: Metadata and master entity data must be available.

### Available Filters:
- No filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- `MasterKeyFields`: Default is an empty string.
- `DataPacketName`: Default is `'EbEntityLink'`.
- `PropertyName`: Default is `'old'`.
- `FrameType`: Default is `frtDetail`.
- `ShowActionPanel`: Default is `False`.
- `AvailableActions`: Default is an empty string.

### Field Validation and Conditions:
- No explicit field validations or conditions are defined in the code.

---

## 5. Main Functions:

### `Create` Constructor:
- **Purpose**: Initializes the frame with specific properties to prevent automatic updates of detail data and configure it to display old values.
- **Business Logic**:
  - Prevents master-detail automatic updates by setting `MasterKeyFields` to an empty string.
  - Configures the frame to display old values using `DataPacketName` and `PropertyName`.
  - Disables the action panel and available actions.

---

## 6. API Service Consumption:

- No external API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: Used for SOAP-based web service communication.
- **DB and DBClient**: Used for database interactions.

### Custom Components:
- **TFRAMEBaseCtrlEditSOA**: The base class for the frame, likely providing common functionality for editing and managing data.

---

## 9. Fields and Validations Listing:

- **MasterKeyFields** (type: string, default: empty, optional): Prevents automatic updates by leaving it empty.
- **DataPacketName** (type: string, default: `'EbEntityLink'`, required): Specifies the detail name in the metadata.
- **PropertyName** (type: string, default: `'old'`, required): Points to the field containing the details in the master entity.
- **FrameType** (type: enum, default: `frtDetail`, required): Configures the frame as a detail frame.
- **ShowActionPanel** (type: boolean, default: `False`, optional): Hides the action panel.
- **AvailableActions** (type: string, default: empty, optional): Disables all actions.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Create Frame] --> [Set Properties] --> [Display Old Values] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Frame Initialization --> Set Properties --> Display Old Values
```

### Code Snippets:
```delphi
var
  Frame: TFRAMEebEntityLinkOldValue;
begin
  Frame := TFRAMEebEntityLinkOldValue.Create(Self);
  // Frame is now configured to display old values
end;
```

### Screenshots:
Not applicable as the `.dfm` file is not provided.

---

## 11. Important Comments in the Code:

- **Comment**: Explains why `MasterKeyFields` is left empty:
  ```plaintext
  // masterfields fica com nenhum campo para evitar a actualização automática master/detail
  // principalmente pq quase todos os campos da entidade são keys.
  ```

- **Comment**: Describes the purpose of `DataPacketName` and `PropertyName`:
  ```plaintext
  // o nome do detail no datapacket(metadata) é sempre no singular
  // nome do campo da metadata(entidadeMaster) que vai conter os details
  ```

---

## 12. Conclusion:

The `TFRAMEebEntityLinkOldValue` frame is a specialized component designed to display static old values of an entity in a master-detail relationship. Its strength lies in its ability to prevent automatic updates of detail data, ensuring data integrity. However, it lacks user interaction capabilities and error handling mechanisms.

---

## 13. Short Summary:

The `TFRAMEebEntityLinkOldValue` frame displays static old values of an entity in a master-detail relationship, preventing automatic updates. It is configured with specific properties to ensure data integrity and is suitable for scenarios where historical data needs to remain unchanged.#### **FRebEntityLinkOldValue.pas**

```
unit FRebEntityLinkOldValue;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel;

type
  TFRAMEebEntityLinkOldValue = class(TFRAMEBaseCtrlEditSOA)
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent);  override;
    { Public declarations }
  end;

var
  FRAMEebEntityLinkOldValue: TFRAMEebEntityLinkOldValue;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes;

constructor TFRAMEebEntityLinkOldValue.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  // masterfields fica com nenhum campo para evitar a actualiza��o automatica master/detail
  // principalmente pq quase todos os campos da entidade s�o keys.
  // ou seja, por comportamento esperado, o master actualiza seus clients que se relacionam pelas suas keys.
  // e neste caso, como n�o queremos que os dados nesta frame seja alterados, simplesmente n�o colocamos os campos q relacionam.
  MasterKeyFields := '';
  DataPacketName := 'EbEntityLink';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'old';         // nome do campo da metadata(entidadeMaster) que vai conter os details
  FrameType := frtDetail;

  ShowActionPanel := False;
  AvailableActions := '';
end;

end.
```

#### **FRebEntityLinkOldValue.dfm**

```
inherited FRAMEebEntityLinkOldValue: TFRAMEebEntityLinkOldValue
end
```
<!-- tabs:end -->

