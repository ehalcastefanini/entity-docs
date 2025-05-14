<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar uma lista de mercados associados a agentes. Ele permite adicionar, excluir e editar mercados vinculados a agentes, além de realizar buscas por mercados. O objetivo é facilitar a manipulação e visualização de dados relacionados a mercados de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do componente.
  - Componentes visuais como `TcxGrid`, `TcxEditRepositoryButtonItem` e `TcxGridDBTableView` para a interface gráfica.
  - Serviços SOAP para integração com dados externos.
  - Manipulação de datasets para exibição e edição de dados.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `market`: Código do mercado (string).
      - `marketName`: Nome do mercado (string).
      - `agent`: Código do agente (string, oculto).
    - **Ações do Grid e seus Efeitos:**
      - Adicionar (`ADD`): Adiciona um novo mercado.
      - Excluir (`DELETE`): Remove um mercado existente.
      - Editar: Permite alterar valores diretamente no grid.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um mercado.
  - Excluir um mercado.
  - Editar informações de um mercado.
  - Buscar mercados por código ou descrição.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato de tabela.
  - `TcxEditRepositoryButtonItem`: Botão para realizar buscas.
  - Métodos como `m_FindMarket` e `UpDateAgentMarkets` para manipulação de dados.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão de busca: `if botão clicado then abrir diálogo de busca`.
  - Evento `OnEditValueChanged` do grid: `if valor do campo alterado then validar e atualizar dados`.
  - Ação `ACTaddExecute`: `if botão adicionar clicado then criar novo mercado`.
  - Ação `ACTdeleteExecute`: `if botão excluir clicado then remover mercado selecionado`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente com o método `Create`.
  2. Configuração do grid, incluindo campos somente leitura, campos ocultos e ordem de exibição.
  3. Interação do usuário:
     - Clique no botão de busca para abrir o diálogo de busca.
     - Alteração de valores no grid para editar dados.
     - Clique nos botões de adicionar ou excluir para manipular mercados.

* **Dados Necessários:**
  - Código do mercado.
  - Nome do mercado.
  - Código do agente (associado automaticamente).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Adicionar: Requer que os campos obrigatórios sejam preenchidos.
  - Excluir: Requer que um mercado esteja selecionado no grid.

* **Filtros Disponíveis:**
  - Busca por código do mercado.
  - Busca por descrição do mercado.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Mercado não encontrado" se a busca não retornar resultados.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Campo `market`: Deve ser único e não vazio.
  - Campo `marketName`: Deve ser preenchido.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Create`: Inicializa o componente e configura o grid.
  - `m_FindMarket`: Abre o diálogo de busca para selecionar um mercado.
  - `UpDateAgentMarkets`: Atualiza a lista de mercados associados ao agente.
  - `ACTaddExecute`: Adiciona um novo mercado.
  - `ACTdeleteExecute`: Remove o mercado selecionado.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CustomerMarketServiceUtils`.
  - Finalidade: Manipular dados de mercados associados a agentes.
  - Dados enviados e recebidos não estão explicitamente definidos no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxEditRepository`, `SOAPHTTPClient` para interface gráfica e integração SOAP.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para o componente.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `market` (string, obrigatório).
  - `marketName` (string, obrigatório).
  - `agent` (string, oculto).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `market` → `marketCode`.
  - `marketName` → `description`.
  - `agent` → `agent`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  FRAMElistMarkets := TFRAMElistMarkets.Create(Self);
  FRAMElistMarkets.Parent := Self;
  FRAMElistMarkets.Show;
  ```
* **HTML Representando o Grid:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>Market</th>
        <th>Market Name</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>001</td>
        <td>Market A</td>
      </tr>
      <tr>
        <td>002</td>
        <td>Market B</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração do grid no método `Create`:
  - Define campos somente leitura, ocultos e ordem de exibição.
* Uso do método `m_FindMarket` para busca de mercados.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar mercados associados a agentes, com funcionalidades de busca, adição, exclusão e edição. No entanto, faltam detalhes sobre validações e mensagens de erro específicas, que poderiam melhorar a experiência do usuário.

---

## 13. Resumo Curto:

O componente `TFRAMElistMarkets` gerencia mercados associados a agentes, permitindo busca, adição, exclusão e edição de dados. Ele utiliza um grid interativo e integra-se a serviços SOAP para manipulação de dados.#### **FRlistMarkets.pas**

```
unit FRlistMarkets;

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
  TFRAMElistMarkets = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTdeleteExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindMarket(Sender: TObject; AButtonIndex: Integer);
    procedure UpDateAgentMarkets;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMElistMarkets: TFRAMElistMarkets;

//const
//  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneUtils, kneFindDialog, kneDialogFactory, Global, kneFGFindUtils,
  kneFREditSOA, kneConfigObjects,
  //Forms
  Magents,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMElistMarkets.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'Code=agent';
  DataPacketName := 'AgentMarket';
  PropertyName := 'markets';
  FrameType := frtDetail;

  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................ 
    DefineReadOnlyFields('marketName;');
    // Campos Hidden ...........................................................
    DefineHiddenFields('agent;');
    // Ordem Campos ............................................................
    DefineOrderFields('market; marketName;');
    // Key Fields ..............................................................
    KeyFields:= 'agent;market';
    // Custom Editors ..........................................................
    AddCustomField('market','cxEDTfindMarket');
  end; //with

  cxEDTfindMarket.Properties.OnButtonClick := m_FindMarket;

end;

//---------------- Finds
procedure TFRAMElistMarkets.m_FindMarket(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
  lv_AgentMarketsList: string;
begin

  try    // inicializa��o de Find Dialog
    lv_Find := nil;
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    // campos para selec��o do Find DataSet
    lv_Find.Options.DataSelection.FieldNameForCode:= 'marketCode';                        //TODO: JAR 19-07-2007 Verificar o nome dos campos (m_FindProductGrade)
    lv_Find.Options.DataSelection.FieldNamesForDesc.Clear;
    lv_Find.Options.DataSelection.FieldNamesForDesc.Add('description');

    lv_Find.Options.DataSelection.TargetDataSet:= CDStable;
```

#### **FRlistMarkets.dfm**

```
inherited FRAMElistMarkets: TFRAMElistMarkets
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
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
  end
end
```
<!-- tabs:end -->

